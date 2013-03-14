package lancet.api

import lancet.core._
import lancet.interpreter._

object Lancet {

  def setScalac(global: scala.tools.nsc.Global) = ScalaCompile.setCompiler(global)

  def newInterpreter = { 
    val it = new BytecodeInterpreter_Exec
    it.initialize
    it
  }

/*
  def newCompilerSimple = { 
    val it = new BytecodeInterpreter_Simple
    it.initialize
    it
  }
*/

  def newCompilerOpt = { 
    val it = new BytecodeInterpreter_LIR_Opt with DefaultMacros
    it.initialize
    it
  }

  def newInstance(global: scala.tools.nsc.Global) = { 
    setScalac(global)
    newCompilerOpt
  }




  // method and field hints (unfold/macro, frozen/stable, etc..)


  // quote/unquote (staging)

  // likely, speculate, stable


  // decompile/recompile


}



import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime



trait DefaultMacros extends BytecodeInterpreter_LIR_Opt { self =>

    // *** macro interface

    def reset[A](f: => A): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def shift[A,B](f: (A=>B) => B): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def slowpath(): Unit = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def speculate(x: Boolean): Boolean = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well



    // *** macro implementations
    type SlowpathFrame = AnyRef

    val it = Lancet.newInterpreter
    it.TRACE = true
    it.TRACE_BYTE_CODE = true

    def mkInterpreterFrame(locals: Array[AnyRef], bci: Int, tos: Int, method: ResolvedJavaMethod, parent: SlowpathFrame): SlowpathFrame = {

      // encapsulation -- shouldn't do this calculation here
      import InterpreterFrame.BASE_LENGTH
      val additionalStackSpace = locals.length - (method.getMaxLocals() + method.getMaxStackSize() + BASE_LENGTH)
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
      val res = it.popAsObject(root, root.getMethod.getSignature.getReturnKind()) // should take actual method (root-1) frame

      Console.println("result: " + res)
      res

      // pass it on to compiler result
      // need to abort -- throw new InterpreterException
    }


    // *** global interpreter interface
    def interpret[A:Manifest,B:Manifest](f: A=>B): A=>B = { arg =>
      val meth = f.getClass.getMethod("apply", manifest[A].erasure)
      val res = it.execute(meth, Array[Object](f,arg.asInstanceOf[Object]))
      res.asInstanceOf[B]
    }

    def exec[B:Manifest](f: => B): B = compile[Int,B](x => f).apply(0)


    def decompileInternal[A:TypeRep,B:TypeRep](f: Rep[Object]): (Rep[Object],Block[Object]) = {
      val arg = Dyn[Object](fresh)
      val body = reify {
        val Partial(fs) = eval(f)
        val Static(cls: Class[_]) = fs("clazz")
        withScope {
          //println("{ object BODY {")
          emitString("  var RES = null.asInstanceOf["+typeRep[B]+"]")
          execute(cls.getMethod("apply", classOf[Object]), Array[Rep[Object]](f,arg)(repManifest[Object]))
          //println("}")
          //"BODY.RES.asInstanceOf["+typeRep[B]+"]}"
          Dyn[Object]("RES.asInstanceOf["+typeRep[B]+"]")
        }
      }
      (arg,body)
    }

    def decompileInternal0[B:TypeRep](f: Rep[Object]): Block[Object] = {
      val arg = Dyn[Object](fresh)
      val body = reify {
        val Partial(fs) = eval(f)
        val Static(cls: Class[_]) = fs("clazz")
        withScope {
          //println("{ object BODY {")
          emitString("  var RES = null.asInstanceOf["+typeRep[B]+"]")
          execute(cls.getMethod("apply"), Array[Rep[Object]](f)(repManifest[Object]))
          //println("}")
          //"BODY.RES.asInstanceOf["+typeRep[B]+"]}"
          Dyn[Object]("RES.asInstanceOf["+typeRep[B]+"]")
        }
      }
      body
    }


    def decompileDelimited[A:TypeRep,B:TypeRep](parent: InterpreterFrame, delim: InterpreterFrame): (Rep[Object],Block[Object]) = {
      val arg = Dyn[Object](fresh)
      val body = reify {
        withScope {
          emitString("  var RES = null.asInstanceOf["+typeRep[B]+"]")
          //Console.println("delim: "+contextKey(parent) + "\n" + contextKey(delim))
          /*def rec(frame: InterpreterFrame): InterpreterFrame = {
            Console.println("rec: "+frame)
            Console.println("rec: "+contextKey(frame))
            if (contextKey(frame) == contextKey(delim)) frame.getTopFrame.asInstanceOf[InterpreterFrame]
            else { 
              frame.setParentFrame(rec(frame.getParentFrame)); frame 
            }
          }
          val frame = rec(parent.copy)*/
          val frame = parent.copy
          val top = frame.getTopFrame.asInstanceOf[InterpreterFrame]
          frame.setBCI(frame.getNextBCI)
          //pushAsObject(arg,)
          pushAsObject(frame, frame.getMethod.getSignature().getReturnKind(), arg)
          executeRoot(top,frame)
          //popAsObject(frame, delim.getMethod.getSignature.getReturnKind())
          Dyn[Object]("RES.asInstanceOf["+typeRep[B]+"]")
        }
      }
      (arg,body)
    }

    var traceMethods = false

    def printcode = println(lastcode)

    var resetStack: List[InterpreterFrame] = Nil

    def handleMethodCall(parent: InterpreterFrame, m: ResolvedJavaMethod): Option[InterpreterFrame] = {
      val className = m.getDeclaringClass.toJava.getName
      val fullName = className + "." + m.getName
      var continuation: InterpreterFrame = parent
      def handle(f: List[Rep[Object]] => Rep[Object]): Option[InterpreterFrame] = {
        val returnValue = f(popArgumentsAsObject(parent, m, !java.lang.reflect.Modifier.isStatic(m.getModifiers)).toList)
        pushAsObject(continuation, continuation.getMethod.getSignature().getReturnKind(), returnValue)
        Some(if (continuation == parent) null else continuation)
      }

      if (traceMethods) Console.println("// "+fullName)

      // check for known methods
      fullName match {
        case "lancet.api.DefaultMacros.shift" => handle {
          case r::f::Nil => 
            val reset = null
            //val reset = resetStack.head
            //continuation = reset
            continuation = getContext(parent).reverse.tail.head // we're inside reset's scope, abort full
            emitString("//begin shift")
            //emitString("// looking for delimiter "+contextKey(reset))
            
            val (argk,blockk) = decompileDelimited[Int,Int](parent,reset)
            val (args,blocks) = decompileInternal[Int,Int](f)

            reflect[Unit]("def k",args,"(",argk,":","Int","): Int = ", blockk)
            emitString("val "+args+" = k"+args+" _")

            val res = reflect[Int](blocks)

            emitString("//end shift")
            res.asInstanceOf[Rep[Object]]
        }

        case "lancet.api.DefaultMacros.reset" => handle {
          case r::f::Nil => 
            emitString("//begin reset")
            val save = resetStack
            continuation.setBCI(continuation.getNextBCI) // XXX
            //emitString("// install delimiter "+contextKey(continuation))
            resetStack = continuation::resetStack
            val block = decompileInternal0[Int](f)
            val res = reflect[Int](block)
            emitString("//end reset")
            resetStack = save
            res.asInstanceOf[Rep[Object]]
        }
      

        case "lancet.api.DefaultMacros.interpret" => handle {
          case r::f::m1::m2::Nil => 
            val self = liftConst(this)
            reflect[Object](self,".interpret(",f,")")
        }
        case s if s.endsWith(".fun") => handle {
          case r::f::m1::m2::Nil => 
            val self = liftConst(this)
            reflect[Object](self,".fun(",f,")")
        }


        case "lancet.api.DefaultMacros.slowpath" => handle {
          case r::Nil => 
            val self = liftConst(this)
            // get caller frame from compiler ('parent')
            // emit code to construct interpreter state
            def rec(frame: InterpreterFrame): Rep[Object] = if (frame == null) liftConst(null) else {
              val p = rec(frame.getParentFrame)
              val frame1 = frame.asInstanceOf[InterpreterFrame_Str]
              reflect[Object](self,".mkInterpreterFrame(Array[Object]("+
                frame1.locals.map(x=>x+".asInstanceOf[AnyRef]").mkString(",")+"), "+ // cast is not nice
                frame1.nextBci+", "+ // need to take *next* bci (cur points to call!)
                frame1.getStackTop()+", "+
                liftConst(frame1.getMethod)+", "+p+")")
            }
            val frame = rec(parent)

            // discard compiler state
            val callers = getContext(parent)
            continuation = callers.reverse.tail.head // second from top! copy or not?

            /* NOTE: correctly unwinding the stack would also mean unlocking monitors and
            calling .dispose on frames)*/

            // exec interpreter to resume at caller frame

            val typ = continuation.getMethod.getSignature.getReturnKind // FIXME
            val res = reflect[Int](self,".execInterpreter("+frame+").asInstanceOf[Int] // drop into interpreter")

            emitString("// old parent: " + contextKey(parent))
            emitString("// new parent: " + contextKey(continuation))

            res.asInstanceOf[Rep[Object]]
        }
        case "scala.runtime.BoxesRunTime.boxToInteger" => handle {
          case r::Nil => reflect[Integer](r,".asInstanceOf[Integer]")
        }
        case "scala.runtime.BoxesRunTime.unboxToInt" => handle {
          case r::Nil => reflect[Integer](r,".asInstanceOf[Int]")
        }
        case "scala.Predef$.println" => handle {
          case r::Nil => reflect[Object]("println(",r,")")
        }
        case _ => 
          //println(fullName)
          None
      }
    }


    override def isSafeRead(base: Object, offset: Long, field: ResolvedJavaField, typ: TypeRep[_]): Boolean =
      super.isSafeRead(base, offset, field, typ) || {
        val name = field.getDeclaringClass.toJava.getName + "." + field.getName
        name match {
          case _ =>
           false
        }
      }


    override def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame =
      handleMethodCall(parent,m).getOrElse(super.resolveAndInvoke(parent, m))

    override def invokeDirect(parent: InterpreterFrame, m: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame =
      handleMethodCall(parent,m).getOrElse(super.invokeDirect(parent, m, hasReceiver))


}