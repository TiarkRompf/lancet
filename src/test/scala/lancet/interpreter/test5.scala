package lancet
package interpreter

import lancet.api._

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

class TestInterpreter5 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-5"


  def test1 = withOutFileChecked(prefix+"slowpath1") {
    val it = new Decompiler

    def compute(i: Int) = if (i == 50) it.dropdead

    val f = it.compile { (x:Int) => 
      var i = 0
      while (i < x) {
        compute(i)
        i += 1
      }
      i
    }
    println(f(100))
  }


  class Decompiler extends BytecodeInterpreter_Opt {

    def dropdead(): Unit = assert(false, "needs to be compiled with LancetJIT")

    /*def decompileInternal[A:TypeRep,B:TypeRep](f: Rep[Object]): (Rep[Object],Block[Object]) = {
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
    }*/

    def handleMethodCall(parent: InterpreterFrame, m: ResolvedJavaMethod): Boolean = {
      val className = m.holder.toJava.getName
      val fullName = className + "." + m.name
      def handle(f: List[Rep[Object]] => Rep[Object]): Boolean = {
        val returnValue = f(popArgumentsAsObject(parent, m, !java.lang.reflect.Modifier.isStatic(m.accessFlags)).toList)
        pushAsObject(parent, m.signature().returnKind(), returnValue)
        true
      }

      Console.println(fullName)

      // check for known collection methods
      fullName match {
        case "lancet.interpreter.TestInterpreter5$Decompiler.dropdead" => handle {
          case r::Nil => reflect[Object]("() // drop into interpreter")
        }
        case _ => 
          //println(fullName)
          false
      }
    }


    override def isSafeRead(base: Object, offset: Long, field: ResolvedJavaField, typ: TypeRep[_]): Boolean = {
    super.isSafeRead(base, offset, field, typ) || {
      val name = field.holder.toJava.getName + "." + field.name
      name match {
        case _ =>
         false
      }
    }}



    // TODO: do we need to reconstruct generic types, i.e. Seq[A] ?

    override def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame =
      if (handleMethodCall(parent,m)) null else super.resolveAndInvoke(parent, m)
    override def invokeDirect(parent: InterpreterFrame, m: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame =
      if (handleMethodCall(parent,m)) null else super.invokeDirect(parent, m, hasReceiver)


    initialize()
    emitUniqueOpt = true
    debugBlockKeys = false
    debugReadWrite = false

  }

}
