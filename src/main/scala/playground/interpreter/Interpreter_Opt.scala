package playground.interpreter

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;

import scala.collection.{mutable,immutable}

class BytecodeInterpreter_Opt0 extends BytecodeInterpreter_Str with RuntimeUniverse_Opt {
    override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m)
    override def objectGetClass(receiver: Rep[Object]): Option[Class[_]] = {
      eval(receiver) match {
        case Partial(fs) => 
          val Const(clazz: Class[_]) = eval(fs("clazz"))
          Some(clazz)
        case Const(x) =>
          val clazz = x.getClass
          Some(clazz)
        case _ =>
          None
      }        
    }




    var worklist: IndexedSeq[InterpreterFrame] = Vector.empty

    var budget = 50000

    var emitControlFlow = true
    var emitRecursive = false

    def exec(frame: InterpreterFrame): Rep[Unit] = { // called internally to initiate control transfer
      
      if (budget <= 0) {
        println("// *** BUDGET EXCEEDED ***")
        return unit(().asInstanceOf[Object]).asInstanceOf[Rep[Unit]]
      }

      if (frame.getParentFrame == null) { // TODO: cleanup?
        val p = popAsObject(frame, frame.getMethod.signature.returnKind())
        return reflect[Unit]("(RES = "+p+") // return to root")
      }

      val method = frame.getMethod()
      if (!emitRecursive && getContext(frame).drop(1).exists(_.getMethod() == method)) { // recursive (TODO: faster test)
        println("// *** RECURSIVE: "+method+" ***")
        return reflect[Unit]("throw new Exception(\"RECURSIVE: "+frame.getMethod+"\")")
      }

      budget -= 1
      
      // decision to make: explore block afresh or generate call to existing onep

      worklist = worklist :+ (frame.asInstanceOf[InterpreterFrame_Str].copy)

      if (emitControlFlow && worklist.tail.nonEmpty)
        reflect[Unit]("goto "+contextKey(frame))
      else
        unit(().asInstanceOf[Object]).asInstanceOf[Rep[Unit]]
    }


    // TODO: can't translate blocks just like that to Scala methods: 
    // next block may refer to stuff defined in earlier block (need nesting, 
    // but problem with back edges)

    // may need different treatment for intra-procedural blocks and function
    // calls: want to inline functions but not generally duplicate local blocks

    def loop(root: InterpreterFrame, main: InterpreterFrame): Unit = {// throws Throwable {

      pushAsObjectInternal(root, main.getMethod.signature().returnKind(), reflect[Object]("null // stub return value "+main.getMethod.signature().returnKind())); // TODO: cleanup?

      val info = new scala.collection.mutable.HashMap[String, Int]

      while (worklist.nonEmpty) {
        var frame = worklist.head
        worklist = worklist.tail

        val key = contextKey(frame)
        val seen = info.getOrElse(contextKey(frame), 0)

        info(key) = seen + 1

        if (seen > 0) {
          println("// *** SEEN " + seen + ": " + key)
        }

        val seenEnough = seen > 3  // TODO: this is just a random cutoff, need to do fixpoint iteration

        if (!seenEnough && frame.getParentFrame != null) {
          val bci = frame.getBCI()
          val bs = new BytecodeStream(frame.getMethod.code())
          //bs.setBCI(globalFrame.getBCI())

          //def frameStr(frame: InterpreterFrame) = getContext(frame).map(frame => ("" + frame.getBCI + ":" + frame.getMethod() + frame.getMethod().signature().asString()).replace("HotSpotMethod",""))

          if (emitControlFlow) {
            println("// *** begin block " + key)
            //println("// *** stack " + frame.asInstanceOf[InterpreterFrame_Str].locals.mkString(","))
          }
          executeBlock(frame, bs, bci)
        } else {
          if (seenEnough) println("// *** seen enough")
        }
      }
    }
}





class BytecodeInterpreter_Opt extends BytecodeInterpreter_Str with RuntimeUniverse_Opt {
    override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m)
    override def objectGetClass(receiver: Rep[Object]): Option[Class[_]] = {
      eval(receiver) match {
        case Partial(fs) => 
          val Const(clazz: Class[_]) = eval(fs("clazz"))
          Some(clazz)
        case Const(x) =>
          val clazz = x.getClass
          Some(clazz)
        case _ =>
          None
      }        
    }




    var worklist: IndexedSeq[InterpreterFrame] = Vector.empty

    var budget = 5000

    var emitControlFlow = true
    var emitRecursive = false

    val info = new mutable.HashMap[String, Int] // map key to id
    var count = 0

    val storeInfo = new mutable.HashMap[String, StoreLattice.Elem] // map key to store
    val frameInfo = new mutable.HashMap[String, FrameLattice.Elem] // map key to store

    val srcInfo = new mutable.ArrayBuffer[List[String]](budget)

    for (i <- 0 until budget) srcInfo += Nil

    // helpers

    def freshFrame(frame: InterpreterFrame): InterpreterFrame_Str = if (frame eq null) null else {
      val frame2 = frame.asInstanceOf[InterpreterFrame_Str].copy2(freshFrame(frame.getParentFrame))
      val depth = frame2.depth
      
      def copyTypeRep(x: Rep[AnyRef]): TypeRep[AnyRef] = (x match { 
        case null => typeRep[Any] 
        case x => x.typ
      }).asInstanceOf[TypeRep[AnyRef]]

      // use fresh argument symbols
      for (i <- 0 until frame2.locals.length)
        frame2.locals(i) = new Dyn[Object]("p"+depth+"_"+i)(copyTypeRep(frame2.locals(i)))

      frame2.returnValue = new Dyn[Object]("r")(copyTypeRep(frame2.returnValue))
      frame2
    }

    def getAllArgs(frame: InterpreterFrame) = frame.getReturnValue()::getContext(frame).dropRight(1).flatMap(_.asInstanceOf[InterpreterFrame_Str].locals)


    // exec loop

    def exec(frame: InterpreterFrame): Rep[Unit] = { // called internally to initiate control transfer
      
      if (budget <= 0) {
        println("// *** BUDGET EXCEEDED ***")
        return unit(().asInstanceOf[Object]).asInstanceOf[Rep[Unit]]
      }

      if (frame.getParentFrame == null) { // TODO: cleanup?
        val p = popAsObject(frame, frame.getMethod.signature.returnKind())
        return reflect[Unit]("(RES = "+p+") // return to root")
      }

      val method = frame.getMethod()
      if (!emitRecursive && getContext(frame).drop(1).exists(_.getMethod() == method)) { // recursive (TODO: faster test)
        println("// *** RECURSIVE: "+method+" ***")
        return reflect[Unit]("throw new Exception(\"RECURSIVE: "+frame.getMethod+"\")")
      }

      budget -= 1
      
      // decision to make: explore block afresh or generate call to existing one

      var fresh = false
      val key = contextKey(frame)
      val id = info.getOrElseUpdate(key, { val id = count; count += 1; fresh = true; id })
      var cnt = srcInfo(id).length


      // alternative idea (TODO):
      //  implement lub op on stacks


      // copy whole stack. TODO: not ideal

      val frame2 = freshFrame(frame)

      val params = getAllArgs(frame2)
      val args = getAllArgs(frame)

      val extra = store collect { case (k,Partial(fs)) => k }

      // check if store lattice has changed ...
      val store2 = StoreLattice.alpha(store, args, params)

      val stOld = storeInfo.getOrElse(key, StoreLattice.bottom)
      val stNew = StoreLattice.lub(stOld,store2)


      println("// args:   " + args)
      println("// store:  " + store)
      println("// params: " + params)
      println("// store2: " + store2)
      println("// stNew:  " + stNew)

      // decision split vs generalize: pass store2 -> split, stNew -> generalize

      if (stNew != stOld || fresh) { // need to update: enqueue worklist item
        if (stNew != stOld) println("// != old  " + stOld)


        storeInfo(key) = stNew
        worklist = worklist :+ frame2 // TODO: don't enqueue twice
        // note: must not rely on stNew when generating call!

        if (!fresh) cnt += 1
      }

      // role of 'cnt': we do not overwrite speculative results but emit all
      // generated variants. earlier calls still go to the preliminary versions.

      reflect[Unit]("block_"+id+"_"+cnt+"("+args.mkString(","),")(" + extra.map(s=>s+"="+s).mkString(",") + ") // "+key)
    }


    // TODO: can't translate blocks just like that to Scala methods: 
    // next block may refer to stuff defined in earlier block (need nesting, 
    // but problem with back edges)

    // may need different treatment for intra-procedural blocks and function
    // calls: want to inline functions but not generally duplicate local blocks

    def loop(root: InterpreterFrame, main: InterpreterFrame): Unit = {// throws Throwable {

      pushAsObjectInternal(root, main.getMethod.signature().returnKind(), reflect[Object]("null // stub return value "+main.getMethod.signature().returnKind())); // TODO: cleanup?

      while (worklist.nonEmpty) {
        var frame = worklist.head
        worklist = worklist.tail

        val key = contextKey(frame)
        val id = info(key)
        val cnt = srcInfo(id).length

        store = storeInfo(key)

        val (src, _) = captureOutputResult {

          println("// *** begin block " + key)
          val params = getAllArgs(frame)
          val paramsStr = params.map(x => if (x eq null) "?" else x.toString+":"+x.typ)
          val extra = store collect { case (k,Partial(fs)) => k }
          val extraStr = extra.map(x => x+":AnyRef /*= null*/")

          println("def block_"+id+"_"+cnt+"("+paramsStr.mkString(",")+")("+extraStr.mkString(",")+"): Any = {")

          if (frame.getParentFrame != null) { // don't eval root frame -- careful, this is a copy of it!
            val bci = frame.getBCI()
            val bs = new BytecodeStream(frame.getMethod.code())
            //bs.setBCI(globalFrame.getBCI())
            val res = executeBlock(frame, bs, bci)
            println(res)
          } else {
            println("// shouldn't reach here..")
            println("// returned to root")
            println("// rval: " + frame.asInstanceOf[InterpreterFrame_Str].returnValue)
          }

          println("}")
        }

        srcInfo(id) ::= src
      }


      for (i <- 0 until count; x <- srcInfo(i).reverse)
        println(x)//srcInfo(i).head)

    }



}