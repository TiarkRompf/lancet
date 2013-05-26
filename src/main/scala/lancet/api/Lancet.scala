package lancet.api

import lancet.core._
import lancet.interpreter._

import java.lang.annotation._

@Retention(RetentionPolicy.RUNTIME)
@Target(Array(ElementType.FIELD))
class stable extends scala.annotation.Annotation with java.lang.annotation.Annotation {
//scala.annotation.Annotation 
def annotationType = classOf[stable]
}

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
    val it = new BytecodeInterpreter_TIR_Opt with DefaultMacros
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



trait DefaultMacros extends BytecodeInterpreter_TIR_Opt { self =>

    // *** macro interface

    def quote[A:TypeRep](f: => A): Rep[A] = quote0(f) // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def unquote[A](f: => Rep[A]): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    def freezeInt(f: => Int): Int = ??? //unquote(liftConst(f)) // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well
    def freeze[A](f: => A): A = ??? //unquote(liftConst(f)) // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def frozen[A](f: A): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    def reset[A](f: => A): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def shift[A,B](f: (A=>B) => B): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    def slowpath(): Unit = () //??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def fastpath(): Unit = () //??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    def speculate(x: Boolean): Boolean = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def stable[A](x: A): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    // *** macro implementations: runtime callbacks
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
      frame.setNextBCI(bci)
      frame.setStackTop(tos)
      assert(locals.length == frame.locals.length)
      // 0-2 are reserved for parent link, bci etc. don't overwrite
      System.arraycopy(locals, BASE_LENGTH, frame.locals, BASE_LENGTH, locals.length - BASE_LENGTH)
      frame
    }

    def mkCompilerFrame(locals: Array[AnyRef], bci: Int, tos: Int, method: ResolvedJavaMethod, parent: SlowpathFrame): SlowpathFrame = {
      // encapsulation -- shouldn't do this calculation here
      import InterpreterFrame.BASE_LENGTH
      val additionalStackSpace = locals.length - (method.getMaxLocals() + method.getMaxStackSize() + BASE_LENGTH)
      val frame = new InterpreterFrame_Str(method, parent.asInstanceOf[InterpreterFrame_Str], additionalStackSpace);
      frame.setBCI(bci)
      frame.setNextBCI(bci)
      frame.setStackTop(tos)
      assert(locals.length == frame.locals.length)
      val liftedLocals = locals.map {
        case x: Integer => liftConst(x:Int)(typeRep[Int])
        case null => liftConst(null)
        case x: Object => liftConst(x)(typeRep[Object])
      } // FIXME: types!
      // 0-2 are reserved for parent link, bci etc. don't overwrite
      System.arraycopy(liftedLocals, BASE_LENGTH, frame.locals, BASE_LENGTH, locals.length - BASE_LENGTH)
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
    }

    def execCompiler(frame: SlowpathFrame): Object = {
      Console.println("-- start compiling")
      val frame1 = frame.asInstanceOf[InterpreterFrame_Str]
      def dump(f: InterpreterFrame_Str): Unit = if (f != null) {
        Console.println(f.getMethod)
        Console.println(f.locals.drop(InterpreterFrame.BASE_LENGTH).mkString(","))
        dump(f.getParentFrame.asInstanceOf[InterpreterFrame_Str])
      }
      Console.println("frame:")
      dump(frame1)
      val root = frame1.getTopFrame().asInstanceOf[InterpreterFrame_Str]
      Console.println("root:")
      dump(root)

      val tp = root.getMethod.getSignature.getReturnKind()

      val compiled = lms0[Unit,Int] { x => // FIXME: types
        executeRoot(root,frame1)
        //popAsObject(root, tp).asInstanceOf[Rep[Int]]
        reflect[Int]("{println(\"BOO!\");666} // recompiled result -- never seen; not assigned to RES")
      }
      Console.println("-- compiled")

      val res = compiled() // call it!!

      Console.println("result: " + res)
      res:Integer
    }

    // *** global interface
    def interpret[A:Manifest,B:Manifest](f: A=>B): A=>B = { arg =>
      val meth = f.getClass.getMethod("apply", manifest[A].erasure)
      val res = it.execute(meth, Array[Object](f,arg.asInstanceOf[Object]))
      res.asInstanceOf[B]
    }

    def exec[B:Manifest](f: => B): B = compile[Int,B](x => f).apply(0)

    def toFunction[A](x: => A) = ((x:()=>A)=>x).asInstanceOf[{def apply(x: => A):()=>A}](x)

    def quote0[B:TypeRep](x: => B): Rep[B] = {
      //assert(manifest[A] == manifest[Int]) // for now ...
      
      val f = toFunction(x)
      val f1 = decompileInternal0(liftConst(f)(typeRep[Object].asInstanceOf[TypeRep[()=>B]])) // FIXME: type
      // could use decompileFun0, but then we get a conflict with RES values (TODO: incorp. changes from delite)
      reflect[B](f1)
    }


    // *** rep interface

    def decompileFun[A:TypeRep,B:TypeRep](f: Rep[A=>B]): Rep[A] => Rep[B] = {
      //val Partial(fs) = eval(f)
      val Some(cls: Class[_]) = objectGetClass(f);
      { arg => 
        withScope {
          emitString("var RES = null.asInstanceOf["+typeRep[B]+"]")
          execute(cls.getMethod("apply", classOf[Object]), Array[Rep[Object]](f,arg.asInstanceOf[Rep[Object]])(repManifest[Object]))
          Dyn[B]("RES.asInstanceOf["+typeRep[B]+"]")
        }
      }
    }
    def decompileFun0[B:TypeRep](f: Rep[()=>B]): () => Rep[B] = {
      //val Partial(fs) = eval(f)
      val Some(cls: Class[_]) = objectGetClass(f);
      { () => 
        withScope {
          emitString("var RES = null.asInstanceOf["+typeRep[B]+"]")
          execute(cls.getMethod("apply"), Array[Rep[Object]](f)(repManifest[Object]))
          Dyn[B]("RES.asInstanceOf["+typeRep[B]+"]")
        }
      }
    }


    def decompileInternal[A:TypeRep,B:TypeRep](f: Rep[A=>B]): (Rep[A],Block[B]) = {
      val arg = Dyn[A](fresh)
      val body = reify {
        decompileFun(f)apply(arg)
      }
      (arg,body)
    }

    def decompileInternal0[B:TypeRep](f: Rep[()=>B]): Block[B] = {
      val body = reify {
        decompileFun0(f)apply()
      }
      body
    }


    def runDelimited[A:TypeRep,B:TypeRep](frame0: InterpreterFrame)(arg: Rep[A]): Rep[B] = {
      withScope {
        emitString("var RES = null.asInstanceOf["+typeRep[B]+"]")
        val frame = frame0.copy
        val top = frame.getTopFrame.asInstanceOf[InterpreterFrame]
        frame.setBCI(frame.getNextBCI)
        pushAsObject(frame, frame.getMethod.getSignature().getReturnKind(), arg.asInstanceOf[Rep[Object]])
        executeRoot(top,frame)
        Dyn[B]("RES.asInstanceOf["+typeRep[B]+"]") // reflect??
      }
    }

    def decompileDelimited[A:TypeRep,B:TypeRep](frame: InterpreterFrame): (Rep[A],Block[B]) = {
      val arg = Dyn[A](fresh)
      val body = reify {
        runDelimited[A,B](frame)(arg)
      }
      (arg,body)
    }


    // materialize an object
    def evalM[A](x: Rep[A]): A = eval (x) match {
      case Const(x) => x
      case Partial(fs) =>
        val Static(cls: Class[_]) = fs("clazz")

        // if the object is a compile time constant,
        // just return that state. this may or may not
        // be what we want!!
        fs("alloc") match {
          case Static(x:A) => return x
          case _ =>
        }

        // materialize objects recursively
        // to resolve non-constant fields.
        // (question: is this sensible? what if the 
        // graph is really big?)

        val typ = metaAccessProvider.lookupJavaType(cls)
        val obj = it.runtimeInterface.newObject(typ).asInstanceOf[A]

        val fs1 = fs - "clazz" - "alloc"
        val fs2 = typ.getInstanceFields(false) //.find(_.getName == k)

        //println(fs)
        //println(fs2.mkString(","))

        for (fld <- fs2) {
          val name = fld.getName
          val offset = fld.asInstanceOf[HotSpotResolvedJavaField].offset
          val value = evalM(fs1(name))
          it.Runtime.unsafe.putObject(obj, offset, value)
        }
        obj
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
        
        // NOTE: we should take m.getSignature below (see delite test)
        // not quite ... if we're not returning to out continuation we have a different type!
        //println("return "+returnValue+"/"+returnValue.typ+"/"+m.getSignature().getReturnKind)
        //println("to "+continuation.getMethod+"/"+continuation.getMethod.getSignature().getReturnKind)

        //pushAsObject(continuation, m.getSignature().getReturnKind(), returnValue)
        if (continuation == parent)
          pushAsObject(continuation, m.getSignature().getReturnKind(), returnValue)
        else
          pushAsObject(continuation, continuation.getMethod.getSignature().getReturnKind(), returnValue)
        // better guess? take return kind from returnValue.typ??

        Some(if (continuation == parent) null else continuation)
      }

      case class FunR[A:TypeRep,B:TypeRep](frame: InterpreterFrame) {
        def apply(x:Rep[A]): Rep[B] = {
          runDelimited[A,B](frame)(x)
        }
        def decompile: (Rep[A], Block[B]) = {
          decompileDelimited[A,B](frame)
        }
        def fundef: Rep[A=>B] = {
          val (arg,block) = decompileDelimited[A,B](frame)
          reflect[Unit]("def f",arg,"(",arg,":","Int","): Int = ", block)
          reflect[A=>B]("f"+arg+" _")(TypeRep(typeRep[A]+" => "+typeRep[B]))
        }
      }

      def shiftR[A:TypeRep,B:TypeRep](body: FunR[A,B] => Rep[B]): Rep[B] = {
        val k = FunR[A,B](continuation)
        // NOTE: correctly unwinding the stack would also mean exiting monitors
        continuation = getContext(parent).reverse.tail.head // look for matching reset?
        body(k)
      }



      if (traceMethods) Console.println("// "+fullName)

      // check for known methods
      fullName match {
        case "lancet.api.DefaultMacros.shift" => handle {
          case r::(f:Rep[(Int=>Int)=>Int])::Nil => 

            shiftR[Int,Int] { k =>
              emitString("//begin shift")

              val k1 = k.fundef

              val res = reflect[Int](reify{ // protect RES

                decompileFun(f).apply(k1)

              })
              emitString("//end shift")
              res

            }.asInstanceOf[Rep[Object]]
        }

        case "lancet.api.DefaultMacros.reset" => handle {
          case r::(f:Rep[()=>Int])::Nil => 
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
            /*
            shift { k => val k1 = interpreted(k); k1() }
            */
            val self = liftConst(this)
            // get caller frame from compiler ('parent')
            // emit code to construct interpreter state
            def mkInterpreterFrameR(locals: Array[Rep[Object]], bci: Rep[Int], tos: Rep[Int], method: Rep[ResolvedJavaMethod], parent: Rep[InterpreterFrame]): Rep[InterpreterFrame] =
              reflect[InterpreterFrame](self,".mkInterpreterFrame(Array[Object]("+
                locals.map(x=>x+".asInstanceOf[AnyRef]").mkString(",")+"), "+ // cast is not nice
                bci+", "+tos+", "+method+", "+parent+")")

            def execInterpreterR[B:TypeRep](frame: Rep[InterpreterFrame]) = 
              reflect[B](self,".execInterpreter("+frame+").asInstanceOf["+typeRep[B]+"] // drop into interpreter")

            def rec(frame: InterpreterFrame): Rep[InterpreterFrame] = if (frame == null) liftConst(null) else {
              val p = rec(frame.getParentFrame)
              val frame1 = frame.asInstanceOf[InterpreterFrame_Str]
              val bci = /*if (frame1.nextBci < 0) 0 else*/ frame1.nextBci
              mkInterpreterFrameR(frame1.locals,bci,frame1.getStackTop(),liftConst(frame1.getMethod), p)
            }

            def interpreted[A:TypeRep,B:TypeRep](f: FunR[A,B]): Rep[A]=>Rep[B] = 
              (x:Rep[A]) => execInterpreterR[B](rec(f.frame)) // what to do with x:A ??


            shiftR[Unit,Int] { k =>

              val k1 = interpreted(k)
              val res = k1(liftConst())

              emitString("// old parent: " + contextKey(parent))
              emitString("// new parent: " + contextKey(continuation))

              res
            }.asInstanceOf[Rep[Object]]
        }

        case "lancet.api.DefaultMacros.fastpath" => handle {
          case r::Nil => 
            /*
            shift { k => val k1 = compiled(k); k1() }
            */
            val self = liftConst(this)
            // get caller frame from compiler ('parent')
            // emit code to construct interpreter state
            def mkCompilerFrameR(locals: Array[Rep[Object]], bci: Rep[Int], tos: Rep[Int], method: Rep[ResolvedJavaMethod], parent: Rep[InterpreterFrame]): Rep[InterpreterFrame] =
              reflect[InterpreterFrame](self,".mkCompilerFrame(Array[Object]("+
                locals.map(x=>x+".asInstanceOf[AnyRef]").mkString(",")+"), "+ // cast is not nice
                bci+", "+tos+", "+method+", "+parent+")")

            def execCompilerR[B:TypeRep](frame: Rep[InterpreterFrame]) = 
              reflect[B](self,".execCompiler("+frame+").asInstanceOf["+typeRep[B]+"] // drop into freshly compiled")

            def rec(frame: InterpreterFrame): Rep[InterpreterFrame] = if (frame == null) liftConst(null) else {
              val p = rec(frame.getParentFrame)
              val frame1 = frame.asInstanceOf[InterpreterFrame_Str]
              val bci = /*if (frame1.nextBci < 0) 0 else*/ frame1.nextBci
              mkCompilerFrameR(frame1.locals,bci,frame1.getStackTop(),liftConst(frame1.getMethod), p)
            }

            def compiled[A:TypeRep,B:TypeRep](f: FunR[A,B]): Rep[A]=>Rep[B] = 
              (x:Rep[A]) => execCompilerR[B](rec(f.frame)) // what to do with x:A ??

            shiftR[Unit,Int] { k =>

              val k1 = compiled(k)
              val res = k1(liftConst())

              emitString("// old parent: " + contextKey(parent))
              emitString("// new parent: " + contextKey(continuation))

              res
            }.asInstanceOf[Rep[Object]]
        }

        
        case "lancet.api.DefaultMacros.freeze" => handle {
          case r::(f:Rep[()=>Object])::Nil => 
            //println("freeze")
            val obj = evalM(f)
            val block = obj.apply()
            liftConst(block).asInstanceOf[Rep[Object]]
        }
        case "lancet.api.DefaultMacros.freezeInt" => handle {
          case r::(f:Rep[()=>Int])::Nil => 
            //println("freeze")
            val obj = evalM(f)
            val block = obj.apply()
            liftConst[Int] (block).asInstanceOf[Rep[Object]]
        }

        case "lancet.api.DefaultMacros.unquote" => handle {
          case r::(f:Rep[()=>Rep[Int]])::Nil => 
            //println("unquote")
            val obj = evalM(f)
            val block = obj.apply()
            block.asInstanceOf[Rep[Object]]
        }

        case "scala.runtime.BoxesRunTime.boxToInteger" => handle {
          case r::Nil => reflect[java.lang.Integer](r,".asInstanceOf[Integer]")
        }
        case "scala.runtime.BoxesRunTime.unboxToInt" => handle {
          case r::Nil => reflect[Int](r,".asInstanceOf[Int]").asInstanceOf[Rep[Object]]
        }
        /*case "scala.runtime.BoxesRunTime.boxToBoolean" => handle {
          case r::Nil => reflect[java.lang.Boolean](r,".asInstanceOf[Boolean]")
        }*/
        case "scala.Predef$.print" => handle {
          case self::r::Nil => reflect[Unit]("print(",r,")").asInstanceOf[Rep[Object]]
        }
        case "scala.Predef$.println" => handle {
          case self::r::Nil => reflect[Unit]("println(",r,")").asInstanceOf[Rep[Object]]
        }
        case _ => 
          //println(fullName)
          None
      }
    }


    override def isSafeRead(base: Object, offset: Long, field: ResolvedJavaField, typ: TypeRep[_]): Boolean =
      super.isSafeRead(base, offset, field, typ) || {
        val isStable = field.getAnnotation(classOf[stable])
        if (isStable != null)
          println("STABLE READ: "+field)

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










trait DefaultMacrosLMS extends BytecodeInterpreter_LMS_Opt { self =>

    // *** macro interface

    def quote[A:TypeRep](f: => A): Rep[A] = quote0(f) // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def unquote[A](f: => Rep[A]): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    def freeze[A](f: => A): A = ??? //unquote(liftConst(f)) // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def frozen[A](f: A): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    def reset[A](f: => A): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def shift[A,B](f: (A=>B) => B): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    def slowpath(): Unit = () //??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def fastpath(): Unit = () //??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    def speculate(x: Boolean): Boolean = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well

    def stable[A](x: A): A = ??? // assert(false, "needs to be compiled with LancetJIT") should add macro in interpreter as well


    // *** macro implementations: runtime callbacks
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
      frame.setNextBCI(bci)
      frame.setStackTop(tos)
      assert(locals.length == frame.locals.length)
      // 0-2 are reserved for parent link, bci etc. don't overwrite
      System.arraycopy(locals, BASE_LENGTH, frame.locals, BASE_LENGTH, locals.length - BASE_LENGTH)
      frame
    }

    def mkCompilerFrame(locals: Array[AnyRef], bci: Int, tos: Int, method: ResolvedJavaMethod, parent: SlowpathFrame): SlowpathFrame = {
      // encapsulation -- shouldn't do this calculation here
      import InterpreterFrame.BASE_LENGTH
      val additionalStackSpace = locals.length - (method.getMaxLocals() + method.getMaxStackSize() + BASE_LENGTH)
      val frame = new InterpreterFrame_Str(method, parent.asInstanceOf[InterpreterFrame_Str], additionalStackSpace);
      frame.setBCI(bci)
      frame.setNextBCI(bci)
      frame.setStackTop(tos)
      assert(locals.length == frame.locals.length)
      val liftedLocals = locals.map {
        case x: Integer => liftConst(x:Int)(typeRep[Int])
        case null => unit(null)
        case x: Object => liftConst(x)(typeRep[Object])
      } // FIXME: types!
      // 0-2 are reserved for parent link, bci etc. don't overwrite
      System.arraycopy(liftedLocals, BASE_LENGTH, frame.locals, BASE_LENGTH, locals.length - BASE_LENGTH)
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
    }

    def execCompiler(frame: SlowpathFrame): Object = {
      Console.println("-- start compiling")
      val frame1 = frame.asInstanceOf[InterpreterFrame_Str]
      def dump(f: InterpreterFrame_Str): Unit = if (f != null) {
        Console.println(f.getMethod)
        Console.println(f.locals.drop(InterpreterFrame.BASE_LENGTH).mkString(","))
        dump(f.getParentFrame.asInstanceOf[InterpreterFrame_Str])
      }
      Console.println("frame:")
      dump(frame1)
      val root = frame1.getTopFrame().asInstanceOf[InterpreterFrame_Str]
      Console.println("root:")
      dump(root)

      val tp = root.getMethod.getSignature.getReturnKind()

      val compiled = lms0[Unit,Int] { x => // FIXME: types
        executeRoot(root,frame1)
        //popAsObject(root, tp).asInstanceOf[Rep[Int]]
        reflect[Int]("{println(\"BOO!\");666} // recompiled result -- never seen; not assigned to RES")
      }
      Console.println("-- compiled")

      val res = compiled() // call it!!

      Console.println("result: " + res)
      res:Integer
    }

    // *** global interface
    def interpret[A:Manifest,B:Manifest](f: A=>B): A=>B = { arg =>
      val meth = f.getClass.getMethod("apply", manifest[A].erasure)
      val res = it.execute(meth, Array[Object](f,arg.asInstanceOf[Object]))
      res.asInstanceOf[B]
    }

    def exec[B:Manifest](f: => B): B = compile[Int,B](x => f).apply(0)

    def toFunction[A](x: => A) = ((x:()=>A)=>x).asInstanceOf[{def apply(x: => A):()=>A}](x)

    def quote0[B:TypeRep](x: => B): Rep[B] = {
      //assert(manifest[A] == manifest[Int]) // for now ...
      
      val f = toFunction(x)
      val f1 = decompileInternal0(liftConst(f)(typeRep[Object].asInstanceOf[TypeRep[()=>B]])) // FIXME: type
      // could use decompileFun0, but then we get a conflict with RES values (TODO: incorp. changes from delite)
      reflect[B](f1)
    }


    // *** rep interface

    def decompileFun[A:TypeRep,B:TypeRep](f: Rep[A=>B]): Rep[A] => Rep[B] = {
      //val Partial(fs) = eval(f)
      val Some(cls: Class[_]) = objectGetClass(f);
      { arg => 
        withScope {
          emitString("var RES = null.asInstanceOf["+typeRep[B]+"]")
          execute(cls.getMethod("apply", classOf[Object]), Array[Rep[Object]](f,arg.asInstanceOf[Rep[Object]])(repManifest[Object]))
          Dyn[B]("RES.asInstanceOf["+typeRep[B]+"]")
        }
      }
    }
    def decompileFun0[B:TypeRep](f: Rep[()=>B]): () => Rep[B] = {
      //val Partial(fs) = eval(f)
      val Some(cls: Class[_]) = objectGetClass(f);
      { () => 
        withScope {
          emitString("var RES = null.asInstanceOf["+typeRep[B]+"]")
          execute(cls.getMethod("apply"), Array[Rep[Object]](f)(repManifest[Object]))
          Dyn[B]("RES.asInstanceOf["+typeRep[B]+"]")
        }
      }
    }


    def decompileInternal[A:TypeRep,B:TypeRep](f: Rep[A=>B]): (Rep[A],Block[B]) = {
      val arg = fresh[A]
      val body = reify {
        decompileFun(f)apply(arg)
      }
      (arg,body)
    }

    def decompileInternal0[B:TypeRep](f: Rep[()=>B]): Block[B] = {
      val body = reify {
        decompileFun0(f)apply()
      }
      body
    }


    def runDelimited[A:TypeRep,B:TypeRep](frame0: InterpreterFrame)(arg: Rep[A]): Rep[B] = {
      withScope {
        emitString("var RES = null.asInstanceOf["+typeRep[B]+"]")
        val frame = frame0.copy
        val top = frame.getTopFrame.asInstanceOf[InterpreterFrame]
        frame.setBCI(frame.getNextBCI)
        pushAsObject(frame, frame.getMethod.getSignature().getReturnKind(), arg.asInstanceOf[Rep[Object]])
        executeRoot(top,frame)
        Dyn[B]("RES.asInstanceOf["+typeRep[B]+"]") // reflect??
      }
    }

    def decompileDelimited[A:TypeRep,B:TypeRep](frame: InterpreterFrame): (Rep[A],Block[B]) = {
      val arg = fresh[A]
      val body = reify {
        runDelimited[A,B](frame)(arg)
      }
      (arg,body)
    }


    // materialize an object
    def evalM[A](x: Rep[A]): A = eval (x) match {
      case VConst(x) => x
      case Partial(fs) =>
        val Static(cls: Class[_]) = fs("clazz")

        // if the object is a compile time constant,
        // just return that state. this may or may not
        // be what we want!!
        fs("alloc") match {
          case Static(x:A) => return x
          case _ =>
        }

        // materialize objects recursively
        // to resolve non-constant fields.
        // (question: is this sensible? what if the 
        // graph is really big?)

        val typ = metaAccessProvider.lookupJavaType(cls)
        val obj = it.runtimeInterface.newObject(typ).asInstanceOf[A]

        val fs1 = fs - "clazz" - "alloc"
        val fs2 = typ.getInstanceFields(false) //.find(_.getName == k)

        //println(fs)
        //println(fs2.mkString(","))

        for (fld <- fs2) {
          val name = fld.getName
          val offset = fld.asInstanceOf[HotSpotResolvedJavaField].offset
          val value = evalM(fs1(name))
          it.Runtime.unsafe.putObject(obj, offset, value)
        }
        obj
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
        
        // NOTE: we should take m.getSignature below (see delite test)
        // not quite ... if we're not returning to out continuation we have a different type!
        //println("return "+returnValue+"/"+returnValue.typ+"/"+m.getSignature().getReturnKind)
        //println("to "+continuation.getMethod+"/"+continuation.getMethod.getSignature().getReturnKind)

        //pushAsObject(continuation, m.getSignature().getReturnKind(), returnValue)
        if (continuation == parent)
          pushAsObject(continuation, m.getSignature().getReturnKind(), returnValue)
        else
          pushAsObject(continuation, continuation.getMethod.getSignature().getReturnKind(), returnValue)
        // better guess? take return kind from returnValue.typ??

        Some(if (continuation == parent) null else continuation)
      }

      case class FunR[A:TypeRep,B:TypeRep](frame: InterpreterFrame) {
        def apply(x:Rep[A]): Rep[B] = {
          runDelimited[A,B](frame)(x)
        }
        def decompile: (Rep[A], Block[B]) = {
          decompileDelimited[A,B](frame)
        }
        def fundef: Rep[A=>B] = {
          val (arg,block) = decompileDelimited[A,B](frame)
          val nm = "fx"+arg.asInstanceOf[Sym[A]].id
          reflect[Unit]("def ",nm,"(",arg,":","Int","): Int = ", block)
          reflect[A=>B](nm+" _")(TypeRep(typeRep[A]+" => "+typeRep[B]))
        }
      }

      def shiftR[A:TypeRep,B:TypeRep](body: FunR[A,B] => Rep[B]): Rep[B] = {
        val k = FunR[A,B](continuation)
        // NOTE: correctly unwinding the stack would also mean exiting monitors
        continuation = getContext(parent).reverse.tail.head // look for matching reset?
        body(k)
      }



      if (traceMethods) Console.println("// "+fullName)

      // check for known methods
      fullName match {
        case "lancet.api.DefaultMacrosLMS.shift" => handle {
          case r::(f:Rep[(Int=>Int)=>Int])::Nil => 

            shiftR[Int,Int] { k =>
              emitString("//begin shift")

              val k1 = k.fundef

              val res = reflect[Int](reify{ // protect RES
                implicit val mii = TypeRep[Int=>Int]("Int=>Int")
                decompileFun(f).apply(k1)

              })
              emitString("//end shift")
              res

            }.asInstanceOf[Rep[Object]]
        }

        case "lancet.api.DefaultMacrosLMS.reset" => handle {
          case r::(f:Rep[()=>Int])::Nil => 
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
      

        case "lancet.api.DefaultMacrosLMS.interpret" => handle {
          case r::f::m1::m2::Nil => 
            val self = liftConst(this)(mtr)
            reflect[Object](self,".interpret(",f,")")
        }
        case s if s.endsWith(".fun") => handle {
          case r::f::m1::m2::Nil => 
            val self = liftConst(this)(mtr)
            reflect[Object](self,".fun(",f,")")
        }


        case "lancet.api.DefaultMacrosLMS.slowpath" => handle {
          case r::Nil => 
            /*
            shift { k => val k1 = interpreted(k); k1() }
            */
            val self = liftConst[Object](this)(mtr)
            implicit val mif = TypeRep[Object]("InterpreterFrame").asInstanceOf[TypeRep[InterpreterFrame]]
            implicit val mjm = TypeRep[Object]("ResolvedJavaMethod").asInstanceOf[TypeRep[ResolvedJavaMethod]]
            // get caller frame from compiler ('parent')
            // emit code to construct interpreter state
            def mkInterpreterFrameR(locals: Array[Rep[Object]], bci: Rep[Int], tos: Rep[Int], method: Rep[ResolvedJavaMethod], parent: Rep[InterpreterFrame]): Rep[InterpreterFrame] = {
              val xs = locals.flatMap(x=>List(x,".asInstanceOf[AnyRef]",",")).dropRight(1)
              val args = Seq(self,".asInstanceOf[lancet.api.DefaultMacrosLMS].mkInterpreterFrame(Array[Object](") ++ xs ++ 
                         Seq("), ",bci,",",tos,",",method,".asInstanceOf[com.oracle.graal.api.meta.ResolvedJavaMethod],",parent,")")
              reflect[InterpreterFrame](args:_*)
            }

            def execInterpreterR[B:TypeRep](frame: Rep[InterpreterFrame]) = 
              reflect[B](self,".asInstanceOf[lancet.api.DefaultMacrosLMS].execInterpreter(",frame,").asInstanceOf["+typeRep[B]+"] // drop into interpreter")

            def rec(frame: InterpreterFrame): Rep[InterpreterFrame] = if (frame == null) unit(null) else {
              val p = rec(frame.getParentFrame)
              val frame1 = frame.asInstanceOf[InterpreterFrame_Str]
              val bci = /*if (frame1.nextBci < 0) 0 else*/ frame1.nextBci
              mkInterpreterFrameR(frame1.locals,bci,frame1.getStackTop(),liftConst(frame1.getMethod), p)
            }

            def interpreted[A:TypeRep,B:TypeRep](f: FunR[A,B]): Rep[A]=>Rep[B] = 
              (x:Rep[A]) => execInterpreterR[B](rec(f.frame)) // what to do with x:A ??


            shiftR[Unit,Int] { k =>

              val k1 = interpreted(k)
              val res = k1(liftConst())

              emitString("// old parent: " + contextKey(parent))
              emitString("// new parent: " + contextKey(continuation))

              res
            }.asInstanceOf[Rep[Object]]
        }

        case "lancet.api.DefaultMacrosLMS.fastpath" => handle {
          case r::Nil => 
            /*
            shift { k => val k1 = compiled(k); k1() }
            */
            val self = liftConst[Object](this)
            implicit val mif = typeRep[Object].asInstanceOf[TypeRep[InterpreterFrame]]
            implicit val mjm = typeRep[Object].asInstanceOf[TypeRep[ResolvedJavaMethod]]
            // get caller frame from compiler ('parent')
            // emit code to construct interpreter state
            def mkCompilerFrameR(locals: Array[Rep[Object]], bci: Rep[Int], tos: Rep[Int], method: Rep[ResolvedJavaMethod], parent: Rep[InterpreterFrame]): Rep[InterpreterFrame] = {
              val xs = locals.flatMap(x=>List(x,".asInstanceOf[AnyRef]",",")).dropRight(1)
              val args = Seq(self,".asInstanceOf[lancet.api.DefaultMacrosLMS].mkCompilerFrame(Array[Object](") ++ xs ++ 
                         Seq("), ",bci,",",tos,",",method,".asInstanceOf[com.oracle.graal.api.meta.ResolvedJavaMethod],",parent,")")
              reflect[InterpreterFrame](args:_*)
            }

            def execCompilerR[B:TypeRep](frame: Rep[InterpreterFrame]) = 
              reflect[B](self,".asInstanceOf[lancet.api.DefaultMacrosLMS].execCompiler(",frame,").asInstanceOf[",typeRep[B],"] // drop into freshly compiled")

            def rec(frame: InterpreterFrame): Rep[InterpreterFrame] = if (frame == null) unit(null) else {
              val p = rec(frame.getParentFrame)
              val frame1 = frame.asInstanceOf[InterpreterFrame_Str]
              val bci = /*if (frame1.nextBci < 0) 0 else*/ frame1.nextBci
              mkCompilerFrameR(frame1.locals,bci,frame1.getStackTop(),liftConst(frame1.getMethod), p)
            }

            def compiled[A:TypeRep,B:TypeRep](f: FunR[A,B]): Rep[A]=>Rep[B] = 
              (x:Rep[A]) => execCompilerR[B](rec(f.frame)) // what to do with x:A ??

            shiftR[Unit,Int] { k =>

              val k1 = compiled(k)
              val res = k1(liftConst())

              emitString("// old parent: " + contextKey(parent))
              emitString("// new parent: " + contextKey(continuation))

              res
            }.asInstanceOf[Rep[Object]]
        }

        
        case "lancet.api.DefaultMacrosLMS.freeze" => handle {
          case r::(f:Rep[()=>Int])::Nil => 
            //println("freeze")
            val obj = evalM(f)
            val block = obj.apply()
            liftConst(block).asInstanceOf[Rep[Object]]
        }

        case "lancet.api.DefaultMacrosLMS.unquote" => handle {
          case r::(f:Rep[()=>Rep[Int]])::Nil => 
            //println("unquote")
            val obj = evalM(f)
            val block = obj.apply()
            block.asInstanceOf[Rep[Object]]
        }

        case "scala.runtime.BoxesRunTime.boxToInteger" => handle {
          case r::Nil => r//reflect[java.lang.Integer](r,".asInstanceOf[Integer]")
        }
        case "scala.runtime.BoxesRunTime.unboxToInt" => handle {
          case r::Nil => r//reflect[Int](r,".asInstanceOf[Int]").asInstanceOf[Rep[Object]]
        }
        /*case "scala.runtime.BoxesRunTime.boxToBoolean" => handle {
          case r::Nil => reflect[java.lang.Boolean](r,".asInstanceOf[Boolean]")
        }*/
        case "scala.Predef$.print" => handle {
          case self::r::Nil => reflect[Unit]("print(",r,")").asInstanceOf[Rep[Object]]
        }
        case "scala.Predef$.println" => handle {
          case self::r::Nil => reflect[Unit]("println(",r,")").asInstanceOf[Rep[Object]]
        }
        case _ => 
          //println(fullName)
          None
      }
    }


    override def isSafeRead(base: Object, offset: Long, field: ResolvedJavaField, typ: TypeRep[_]): Boolean =
      super.isSafeRead(base, offset, field, typ) || {
        val isStable = field.getAnnotation(classOf[stable])
        if (isStable != null)
          println("STABLE READ: "+field)

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