package lancet
package interpreter

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

class TestInterpreter4 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-4"

  class Marker

  class BytecodeInterpreter_Test extends BytecodeInterpreter_Opt {
    /*override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m) {
      override def isVolatile(field: ResolvedJavaField) = false // don't honor volatile
    }*/

    def handleJsMethod(parent: InterpreterFrame, m: ResolvedJavaMethod): Boolean = {
      val holder = m.holder

      val fullName = m.holder.toJava.getName + "." + m.name

      fullName match {
        case "scala.collection.TraversableLike.filter" => 

          println("// XXX hit filter " + fullName)

          //val receiver = parent.peekReceiver(m)
          val receiver::f::Nil = popArgumentsAsObject(parent, m, true).toList

          val arg = Dyn[Object](fresh)

          val fun = captureOutput {

            val Partial(fs) = eval(f)
            val Static(cls: Class[_]) = fs("clazz")
            execute(cls.getMethod("apply", classOf[Object]), Array[Rep[Object]](f,arg.asInstanceOf[Rep[Object]])(repManifest[Object]))
          }

          val returnValue = reflect[Object](""+receiver+".filter "+ "{ " + arg + "=> " + fun + "}")
          pushAsObject(parent, m.signature().returnKind(), returnValue)
          return true


          false
        case _ => false
      }

      if (classOf[Marker].isAssignableFrom(holder.toJava())) {
        //println("*** XXX JSM " + classOf[Program.JS] + " / " + holder.toJava)
        val receiver = parent.peekReceiver(m)
        val parameters = popArgumentsAsObject(parent, m, true)
        val returnValue = reflect[Object](""+receiver+"."+m.name+"("+parameters.mkString(",")+")")
        pushAsObject(parent, m.signature().returnKind(), returnValue)
        true
      } else false
    }

    override def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame =
      if (handleJsMethod(parent,m)) null else super.resolveAndInvoke(parent, m)
    override def invokeDirect(parent: InterpreterFrame, m: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame =
      if (handleJsMethod(parent,m)) null else super.invokeDirect(parent, m, hasReceiver)
    //override def checkCastInternal(typ: ResolvedJavaType, value: Rep[Object]): Rep[Object] = value // no casts in JavaScript

  }

  object Decompiler extends BytecodeInterpreter_Test {
    initialize()
    emitUniqueOpt = true

    def decompile[A:Manifest,B:Manifest](f: A => B): String = {
      compile(f)
      "<decompiled>"
    }
  }


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


  def test1 = {
    withOutFileChecked(prefix+"decompile1") {  {
      val e = Decompiler.decompile { xs: Seq[Person] =>
        xs.filter(pred1)
      }
      println(e)
    } }
  }

  def test2 = {
    withOutFileChecked(prefix+"decompile2") {  {
      val e = Decompiler.decompile { xs: Seq[Person] =>
        xs.filter(pred2)
      }
      println(e)
    } }
  }
  
  def test3 = {
    withOutFileChecked(prefix+"decompile3") {  {
      val e = Decompiler.decompile { xs: Seq[Person] =>
        xs.filter(pred3)
      }
      println(e)
    } }
  }

  def test4 = {
    withOutFileChecked(prefix+"decompile4") {  {
      val e = Decompiler.decompile { xs: Seq[Person] =>
        xs.map(x => new { val foo = x.age } ).map(x => x.foo)
      }
      println(e)
    } }
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