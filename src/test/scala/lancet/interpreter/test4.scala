package lancet
package interpreter

import lancet.api._

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

class TestInterpreter4 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-4"

  case class Person(val age: Int, val flag: Boolean)

  def pred1(p: Person) = p.age < 42

  def pred2(p: Person) = if (p.flag) p.age < 42 else true

  val Z0 = 0
  val Z1 = 1
  val Z2 = 2
  val Z3 = 3
  val Z4 = 4
  val Z5 = 5
  val Z6 = 6
  val Z7 = 7

  def pred3(p: Person) = p match {
    case Person(Z0, true) => true
    case Person(Z1, _) => true
    case Person(Z2, _) => true
    case Person(Z3, _) => true
    case Person(Z4, _) => true
    case Person(Z5, _) => true
    case Person(Z6, _) => true
    case _ => false
  }


  def test1 = withOutFileChecked(prefix+"decompile1") {
    val dec = new Decompiler
    val e = dec.decompile { xs: Seq[Person] =>
      xs.filter(pred1)
    }
    println(e)
  }

  def test2 = withOutFileChecked(prefix+"decompile2") {
    val dec = new Decompiler
    val e = dec.decompile { xs: Seq[Person] =>
      xs.filter(pred2)
    }
    println(e)
  }
  
  def test3 = withOutFileChecked(prefix+"decompile3") {
    val dec = new Decompiler
    val e = dec.decompile { xs: Seq[Person] =>
      xs.filter(pred3)
    }
    println(e)
  }

  def test4 = withOutFileChecked(prefix+"decompile4") {
    val dec = new Decompiler
    val e = dec.decompile { xs: Seq[Person] =>
      xs.map(x => new { val foo = x.age } ).map(x => x.foo)
    }
    println(e)
  }

  class Marker

  class Decompiler extends BytecodeInterpreter_Opt {

    def decompileInternal[A:TypeRep,B:TypeRep](f: Rep[Object]): (Rep[Object],Block[Object]) = {
      val arg = Dyn[Object](fresh)
      val body = reify {
        val Partial(fs) = eval(f)
        val Static(cls: Class[_]) = fs("clazz")
        withScope {
          //println("{ object BODY {")
          emitString("  var RES = null.asInstanceOf[Object]")
          execute(cls.getMethod("apply", classOf[Object]), Array[Rep[Object]](f,arg)(repManifest[Object]))
          //println("}")
          //"BODY.RES.asInstanceOf["+typeRep[B]+"]}"
          Dyn[Object]("RES.asInstanceOf["+typeRep[B]+"]")
        }
      }
      (arg,body)
    }

    def handleMethodCall(parent: InterpreterFrame, m: ResolvedJavaMethod): Boolean = {
      val className = m.holder.toJava.getName
      val fullName = className + "." + m.getName
      def handle(f: List[Rep[Object]] => Rep[Object]): Boolean = {
        val returnValue = f(popArgumentsAsObject(parent, m, !java.lang.reflect.Modifier.isStatic(m.getModifiers)).toList)
        pushAsObject(parent, m.getSignature().getReturnKind(), returnValue)
        true
      }

      // check for known collection methods
      fullName match {
        case "java.lang.Integer.valueOf" => handle {
          case r::Nil => reflect[Object]("Integer.valueOf("+r+")")
        }
        case "java.lang.Object.getClass" => handle {
          case receiver::Nil =>
            runtimeInterface.objectGetClass(receiver)
        }
        case "scala.runtime.BoxesRunTime.equals" => handle {
          case a::b::Nil => reflect[Boolean](a+"=="+b).asInstanceOf[Rep[Object]]
        }
        case "scala.collection.GenTraversableLike.filter" => handle {
          case receiver::f::Nil =>
            val (arg,body) = decompileInternal[Object,Boolean](f)
            reflect[Object](receiver, ".asInstanceOf[Traversable[Object]].filter ", "{ ("+arg+":"+arg.typ+")" + " => ", body, "}")
        }
        case "scala.collection.generic.FilterMonadic.map" | "scala.collection.TraversableLike.map" => handle {
          case receiver::f::cbf::Nil =>
            val (arg,body) = decompileInternal[Object,Boolean](f)
            reflect[Object](""+receiver, ".asInstanceOf[Traversable[Object]].map ", "{ ("+arg+":"+arg.typ+")" + " => ", body, "}")
        }
        /*case s if s contains ".reflMethod$" => handle { // structural call
          case args =>
            reflect[Object](s+"("+args.mkString(",")+") // structural call")
        }*/
        case "java.lang.Class.getMethod" => handle { // structural call
          case receiver::name::args =>
            reflect[Object](""+receiver+".asInstanceOf[Class[_]].getMethod("+name+".asInstanceOf[String],"+args.map(s=>s+".asInstanceOf[Class[_]]").mkString(",")+")")
        }
        case "java.lang.reflect.Method.invoke" => handle { // structural call
          case receiver::args =>
            reflect[Object](receiver,".asInstanceOf[java.lang.reflect.Method].invoke(",args.mkString(","),")") // FIXME: args!
        }
        case s if className.startsWith("java.lang.reflect") => handle {
          case args =>
            runtimeInterface.invoke(m,args.toArray)
        }
        case s if className.startsWith("scala.runtime.MethodCache") && m.getName != "<init>" => handle {
          case args =>
            runtimeInterface.invoke(m,args.toArray)
        }
        case s if className.startsWith("scala.runtime.EmptyMethodCache") && m.getName != "<init>" => handle {
          case args =>
            runtimeInterface.invoke(m,args.toArray)
        }
        case _ => 
          //println(fullName)
          false
      }
    }


    override def isSafeRead(base: Object, offset: Long, field: ResolvedJavaField, typ: TypeRep[_]): Boolean = {
    super.isSafeRead(base, offset, field, typ) || {
      val name = field.holder.toJava.getName + "." + field.getName
      name match {
        case "scala.collection.generic.GenTraversableFactory.bitmap$0" => true // lazy field, treat as const
        case "scala.collection.generic.GenTraversableFactory.ReusableCBF" => true
        case _ =>
         false
      }
    }}




    override def checkCastInternal(typ: ResolvedJavaType, value: Rep[Object]): Rep[Object] = { // stupid value classes ...
      val y = super.checkCastInternal(typ,value)
      if (typ.toJava == Class.forName("scala.collection.TraversableLike")) {
        exprs = exprs.filter(_._2 != y) // HACK - pretend it didn't happen ...
        reflect[Object](y+".asInstanceOf[Object] // work around value classes, which aren't classes")
      } else y
    }



    // TODO: do we need to reconstruct generic types, i.e. Seq[A] ?

    override def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame =
      if (handleMethodCall(parent,m)) null else super.resolveAndInvoke(parent, m)
    override def invokeDirect(parent: InterpreterFrame, m: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame =
      if (handleMethodCall(parent,m)) null else super.invokeDirect(parent, m, hasReceiver)


    initialize()
    emitUniqueOpt = true
    debugBlockKeys = false
    debugReadWrite = false

    def decompile[A:Manifest,B:Manifest](f: A => B): String = {
      compile(f)
      "<decompiled>"
    }
  }




/* from StatusQuo paper 

List<User> getRoleUser () {
List<User> listUsers = new ArrayList<User>(); 
List<User> users = ... /* Hibernate query */ 
List<Role> roles = ... /* Hibernate query */ 
for (int i = 0; i < users.size(); i++) {
for (int j = 0; j < roles.size(); j++) {
if (users.get(i).roleId == roles.get(j).roleId) {
        User userok = users.get(i);
        listUsers.add(userok);
   }}}
return listUsers; 
}

 List<User> getRoleUser () {
List<User> listUsers = db.executeQuery(
"SELECT u
FROM users u, roles r
WHERE u.roleId == r.roleId ORDER BY u.roleId, r.roleId");
return listUsers; 
}

*/

}