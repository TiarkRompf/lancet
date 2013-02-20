package lancet
package interpreter

import lancet.api._

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

class TestInterpreter5 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-5"


  def test1a = withOutFileChecked(prefix+"slowpath1a") {
    val it = new Decompiler

    def compute(i: Int) = if (i == 50) it.dropdead

    val f = it.interpret { (x:Int) => 
      var i = 0
      while (i < x) {
        compute(i)
        i += 1
      }
      i
    }
    println(f(100))
  }

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
    assert(false)
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
    def dropdead(): Unit = () // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    // macro implementations
    type SlowpathFrame = AnyRef

    val it = Lancet.newInterpreter
    it.TRACE = true
    it.TRACE_BYTE_CODE = true

    def mkInterpreterFrame(locals: Array[AnyRef], bci: Int, tos: Int, method: ResolvedJavaMethod, parent: SlowpathFrame): SlowpathFrame = {

      // encapsulation -- shouldn't do this calculation here
      import InterpreterFrame.BASE_LENGTH
      val additionalStackSpace = locals.length - (method.maxLocals() + method.maxStackSize() + BASE_LENGTH)
      val frame = new it.InterpreterFrame_Exec(method, parent.asInstanceOf[it.InterpreterFrame_Exec], additionalStackSpace);
      frame.setBCI(bci)
      frame.setStackTop(tos)
      assert(locals.length == frame.locals.length)
      // 0-2 are reserved for parent link, bci etc. don't overwrite
      System.arraycopy(locals, BASE_LENGTH, frame.locals, BASE_LENGTH, locals.length - BASE_LENGTH)
      frame

    }

    def execInterpreter(frame: SlowpathFrame): Object = {
      Console.println("-- start interpreting")
      val frame1 = frame.asInstanceOf[it.InterpreterFrame_Exec]
      def dump(f: it.InterpreterFrame_Exec): Unit = if (f != null) {
        Console.println(f.getMethod)
        Console.println(f.locals.drop(InterpreterFrame.BASE_LENGTH).mkString(","))
        dump(f.getParentFrame.asInstanceOf[it.InterpreterFrame_Exec])
      }
      Console.println("frame:")
      dump(frame1)
      val root = frame1.getTopFrame().asInstanceOf[it.InterpreterFrame_Exec]
      Console.println("root:")
      dump(root)
      it.executeRoot(root,frame1)
      Console.println("-- done interpreting")
      dump(frame1) // parent link seems to be erased?
      Console.println("root:")
      dump(root)

      // retrieve value returned to interpreter root frame
      val res = it.popAsObject(root, root.getMethod.signature.returnKind()) // should take actual method (root-1) frame

      Console.println("result: " + res)
      res

      // pass it on to compiler result
      // need to abort -- throw new InterpreterException
    }


    // global interpreter interface
    def interpret[A:Manifest,B:Manifest](f: A=>B): A=>B = { arg =>
      val meth = f.getClass.getMethod("apply", manifest[A].erasure)
      val res = it.execute(meth, Array[Object](f,arg.asInstanceOf[Object]))
      res.asInstanceOf[B]
    }

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

      Console.println("// "+fullName)

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
                frame1.bci+", "+
                frame1.getStackTop()+", "+
                liftConst(frame1.getMethod)+", "+p+")")
            }
            val frame = rec(parent)
            // create interpreter object (or reuse?)
            //val interp = new BytecodeInterpreter_Exec
            // initialize interpreter with corresponding chain of frames
            // exec interpreter to resume at caller frame
            // discard compiler state
            val res = reflect[Object](self,".execInterpreter("+frame+") // drop into interpreter")
            throw new InterpreterException(reflect[Throwable]("new Exception(\"\"+"+res+")"))
        }
        case _ => 
          //println(fullName)
          false
      }
    }


    override def isSafeRead(base: Object, offset: Long, field: ResolvedJavaField, typ: TypeRep[_]): Boolean =
      super.isSafeRead(base, offset, field, typ) || {
        val name = field.holder.toJava.getName + "." + field.name
        name match {
          case _ =>
           false
        }
      }


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
