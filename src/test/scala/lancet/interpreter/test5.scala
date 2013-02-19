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

  def test2 = withOutFileChecked(prefix+"slowpath2") {
    val it = new Decompiler

    abstract class Exp
    case class IntVal(x: Int) extends Exp
    case class DoubleVal(x: Double) extends Exp
    case class StringVal(x: String) extends Exp
    case class Plus(x: Exp, y: Exp) extends Exp
    case class Minus(x: Exp, y: Exp) extends Exp
    case class Var(x: String) extends Exp
    
    abstract class Stm
    case class Assign(x: String, y: Exp) extends Stm
    case class While(x: Exp, y: Stm) extends Stm
    case class Print(x: Exp) extends Stm
    case class Block(xs: List[Stm]) extends Stm

    val env = new scala.collection.mutable.HashMap[String,Any]
    def eval(e: Exp): Any = e match {
      case IntVal(x) => x
      case DoubleVal(x) => x
      case StringVal(x) => x
      case Plus(x,y) => (eval(x),eval(y)) match {
        case (x: Int, y: Int) => x + y
        case (x: Double, y: Double) => x + y
        case (x: String, y: String) => x + y 
        // todo widen
      }
      case Minus(x,y) => (eval(x),eval(y)) match {
        case (x: Int, y: Int) => x + y
        case (x: Double, y: Double) => x + y
        case (x: String, y: String) => x + y 
        // todo widen
      }
      case Var(x) => env(x)
    }
    def exec(s: Stm): Unit = s match {
      case Assign(x,y) => env(x) = eval(y)
      case While(x,y) => while (eval(x).asInstanceOf[Int] > 0) exec(y) // test int
      case Print(x) => println(eval(x))
      case Block(xs) => xs.foreach(exec(_)) // unroll
    }


    val p = Block(List(
      //Assign("max", IntVal(100)),
      Assign("x", IntVal(0)),
      While(Minus(Var("max"), Var("x")), Block(List(
        Print(Var("x")),
        Assign("x", Plus(Var("x"), IntVal(1)))
      )))
    ))

    val f = it.compile { (x:Int) => 
      env("max") = x
      exec(p)
    }
    println(f(100))
  }





  class Decompiler extends BytecodeInterpreter_Opt {

    // macro interface
    def dropdead(): Unit = assert(false, "needs to be compiled with LancetJIT")

    // macro implementations
    trait SlowpathFrame
    def execInterpreter(frame: SlowpathFrame): Unit = {
      val it = Lancet.newInterpreter
      it.TRACE = true
      it.TRACE_BYTE_CODE = true
      val res = it.execute(frame)
    }
    def mkInterpreterFrame(locals: Array[AnyRef], bci: Int, method: ResolvedJavaMethod, parent: SlowpathFrame): SlowpathFrame = ???

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

      // check for known methods
      fullName match {
        case "lancet.interpreter.TestInterpreter5$Decompiler.dropdead" => handle {
          case r::Nil => 
            val self = liftConst(Decompiler.this)
            // get caller frame from compiler ('parent')
            // emit code to construct interpreter state            
            def rec(frame: InterpreterFrame): Rep[Object] = if (frame == null) liftConst(null) else {
              val p = rec(frame.getParentFrame)
              val frame1 = frame.asInstanceOf[InterpreterFrame_Str]
              reflect[Object](self,".mkInterpreterFrame(Array[Object]("+
                frame1.locals.map(x=>x+".asInstanceOf[AnyRef]").mkString(",")+"), "+ // cast is not nice
                frame1.bci+", "+liftConst(frame1.getMethod)+", "+p+")")
            }
            val frame = rec(parent)
            // create interpreter object (or reuse?)
            //val interp = new BytecodeInterpreter_Exec
            // initialize interpreter with corresponding chain of frames
            // exec interpreter to resume at caller frame
            // discard compiler state
            reflect[Object](self,".execInterpreter("+frame+") // drop into interpreter")
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
