package lancet
package interpreter

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

    def decompileInternal[A:TypeRep,B:TypeRep](f: Rep[Object]): (Rep[Object],String) = {
      val arg = Dyn[Object](fresh)
      val body = captureOutput {
        val Partial(fs) = eval(f)
        val Static(cls: Class[_]) = fs("clazz")
        withScope {
          //println("{ object BODY {")
          println("  var RES = null.asInstanceOf[Object]")
          execute(cls.getMethod("apply", classOf[Object]), Array[Rep[Object]](f,arg)(repManifest[Object]))
          //println("}")
          //"BODY.RES.asInstanceOf["+typeRep[B]+"]}"
          "RES.asInstanceOf["+typeRep[B]+"]"
        }
      }
      (arg,body)
    }

    def handleMethodCall(parent: InterpreterFrame, m: ResolvedJavaMethod): Boolean = {
      val fullName = m.holder.toJava.getName + "." + m.name
      def handle(f: List[Rep[Object]] => Rep[Object]): Boolean = {
        val returnValue = f(popArgumentsAsObject(parent, m, !java.lang.reflect.Modifier.isStatic(m.accessFlags)).toList)
        pushAsObject(parent, m.signature().returnKind(), returnValue)
        true
      }

      // check for known collection methods
      fullName match {
        case "java.lang.Integer.valueOf" => handle {
          case r::Nil => reflect[Object]("Integer.valueOf("+r+")")
        }
        case "scala.runtime.BoxesRunTime.equals" => handle {
          case a::b::Nil => reflect[Boolean](a+"=="+b).asInstanceOf[Rep[Object]]
        }
        case "scala.collection.TraversableLike.filter" => handle {
          case receiver::f::Nil =>
            val (arg,body) = decompileInternal[Object,Boolean](f)
            reflect[Object](""+receiver+".asInstanceOf[Traversable[Object]].filter "+ "{ ("+arg+":"+arg.typ+")" + " => " + body + "}")
        }
        case _ => false
      }
    }

    // TODO: do we need to reconstruct generic types, i.e. Seq[A] ?

    override def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame =
      if (handleMethodCall(parent,m)) null else super.resolveAndInvoke(parent, m)
    override def invokeDirect(parent: InterpreterFrame, m: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame =
      if (handleMethodCall(parent,m)) null else super.invokeDirect(parent, m, hasReceiver)


    initialize()
    emitUniqueOpt = true

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