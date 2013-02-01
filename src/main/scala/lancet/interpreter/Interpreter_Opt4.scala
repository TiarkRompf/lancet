package lancet.interpreter

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;

import com.oracle.graal.java.BciBlockMapping

import scala.collection.{mutable,immutable}

// (todo) cse --> elim redundant checks


class BytecodeInterpreter_Opt extends BytecodeInterpreter_Opt4


// version 4 -- reverse engineer more of the program block structure (if, loop)


trait AbstractInterpreter extends BytecodeInterpreter_Str with RuntimeUniverse_Opt {

    override def objectGetClass(receiver: Rep[Object]): Option[Class[_]] = {
      eval(receiver) match {
        case Partial(fs) if fs.contains("clazz") => 
          val Const(clazz: Class[_]) = eval(fs("clazz"))
          Some(clazz)
        case Partial(fs) => 
          val Static(x: NotNull) = fs("alloc") // unsafe? <-- could also set "clazz" field when lifting const
          Some(x.getClass)
        case Const(x: NotNull) =>
          val clazz = x.getClass
          Some(clazz)
        case _ =>
          None
      }        
    }

    object FrameLattice {
      type Elem = InterpreterFrame

      def getFields(x: Elem) = getAllArgs(x).filter(_ != null)

      def lub(x0: Elem, y0: Elem): Elem = { // modifies y!
        if (x0 == null) return null
        val x = x0.asInstanceOf[InterpreterFrame_Str]
        val y = y0.asInstanceOf[InterpreterFrame_Str]

        assert(x.locals.length == y.locals.length, {
          "x.locals " + x.locals.mkString(",") + "\n" + 
          "y.locals " + y.locals.mkString(",") + "\n" + 
          x.getMethod + "\n" + 
          y.getMethod + "\n" + 
          (new Exception getStackTrace ()).mkString("\n")
        })

        assert(x.getMethod == y.getMethod)

        for (i <- 0 until y.locals.length) {
          val a = x.locals(i)
          val b = y.locals(i)

          if (a != b) {
            val str = "PHI_"+x.depth+"_"+i
            if (b == null)
              println("val "+str+" = null.asInstanceOf["+a.typ+"] // LUBC(" + a + "," + b + ")")
            else if (b.toString != str)
              println("val "+str+" = " + b + " // LUBC(" + (if(a==null)a else a + ":"+a.typ)+"," + b + ":"+b.typ+ ")")
            val tp = (if (b == null) a.typ else b.typ).asInstanceOf[TypeRep[AnyRef]] // NPE? should take a.typ in general?
            val phi = Dyn[AnyRef](str)(tp)
            y.locals(i) = phi
          }
        }

        lub(x.getParentFrame, y.getParentFrame)
        y
      }
    }

    // calc lubs and backpatch info for jumps
    type State = (InterpreterFrame, StoreLattice.Elem)    
    def allLubs(states: List[State]): (State,List[String]) = {
      if (states.length == 1) return (states.head, Nil) // fast path
      // backpatch info: foreach state, commands needed to initialize lub vars
      val gos = states map { case (frameX,storeX) =>
        val frameY = freshFrameSimple(frameX)
        var storeY = storeX
        val (go, _) = captureOutputResult { // start with 'this' state, make it match all others
          states foreach { case (f,s) => 
            FrameLattice.lub(f, frameY) 
            storeY = StoreLattice.lub(s,storeY)
          }
          //val locals = FrameLattice.getFields(frameY).filter(_.toString.startsWith("PHI"))
          //val fields = StoreLattice.getFields(storeY).filter(_.toString.startsWith("LUB"))
          //for (v <- locals ++ fields) println("v"+v+" = "+v)
        }
        (go,frameY,storeY)
      }
      val (_,f02,s02) = gos(0)
      for ((_,fx,sx) <- gos) { // sanity check
        assert(contextKey(f02) == contextKey(fx))
        assert(getAllArgs(f02) == getAllArgs(fx))
        assert(s02 == sx, s02+"!=="+sx)
      }
      ((f02,s02),gos.map(_._1))
    }

    def getFields(s: State) = {
      val locals = FrameLattice.getFields(s._1).filterNot(_.isInstanceOf[Static[_]])//filter(_.toString.startsWith("PHI"))
      val fields = StoreLattice.getFields(s._2).filterNot(_.isInstanceOf[Static[_]])//.filter(_.toString.startsWith("LUB"))
      (locals ++ fields).distinct.sortBy(_.toString)
    }

    def statesDiffer(s0: State, s1: State) = 
      getAllArgs(s0._1) != getAllArgs(s1._1) || s0._2 != s1._2

    def freshFrameSimple(frame: InterpreterFrame): InterpreterFrame_Str = if (frame eq null) null else {
      val frame2 = frame.asInstanceOf[InterpreterFrame_Str].copy2(freshFrameSimple(frame.getParentFrame))
      val depth = frame2.depth
      frame2
    }

    def getAllArgs(frame: InterpreterFrame) = frame.getReturnValue()::getContext(frame).dropRight(1).flatMap(_.asInstanceOf[InterpreterFrame_Str].locals)


}


class BytecodeInterpreter_Opt4 extends AbstractInterpreter with BytecodeInterpreter_Str with RuntimeUniverse_Opt {
    override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m)

    // config options

    var debugBlocks = false
    var debugMethods = false
    var debugReturns = false
    var debugLoops = false
    var debugPaths = false
    var debugStats = false

    //var emitControlFlow = true
    var emitRecursive = false

    var budget = 200000

    // global internal data structures

    val graalBlockMapping = new mutable.HashMap[ResolvedJavaMethod, BciBlockMapping] // map key to store
    def getGraalBlocks(method: ResolvedJavaMethod) = graalBlockMapping.getOrElseUpdate(method, {
      val map = new BciBlockMapping(method);
      map.build();
      map
    })

    val stats = new mutable.HashMap[String, Int] // map key to id

    val info = new mutable.HashMap[String, Int] // map key to id
    var count = 0
    def contextKeyId(frame: InterpreterFrame) = {
      val key = contextKey(frame)
      val id = info.getOrElseUpdate(key, { val id = count; count += 1; id })
      (key,id)
    }

    // dynamically scoped internal data structures

    var handler: (InterpreterFrame => Rep[Unit]) = execMethod
    var depth = 0

    def withScope[A](body: =>A): A = { // reset scope, e.g. for nested calls
      val saveHandler = handler
      val saveDepth = depth
      val saveStore = store
      try {
        handler = execMethod
        depth = 0
        //store = StoreLattice.bottom // really clear completely?
        body
      } finally {
        handler = saveHandler
        depth = saveDepth
        store = saveStore 
      }
    }


    // helpers

    def postDominators(blocks: List[BciBlockMapping.Block]) = {
      import scala.collection.JavaConversions._
      var PostDom: Map[BciBlockMapping.Block,Set[BciBlockMapping.Block]] = Map()
      val (exit,internal) = blocks.partition(_.successors.length == 0)
      for (n <- exit)
        PostDom += (n -> Set(n))
      for (n <- internal)
        PostDom += (n -> blocks.toSet)
      var p0 = PostDom
      do {
        p0 = PostDom
        for (n <- internal) {
          val x = (blocks.toSet /: n.successors) ((a:Set[BciBlockMapping.Block],b:BciBlockMapping.Block) => a intersect PostDom(b))
          PostDom += (n -> (Set(n) ++ x))
        }
      } while (PostDom != p0)
      PostDom
    }


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
        println("// *** RECURSIVE: "+method+" *** " + contextKey(frame))
        return reflect[Unit]("throw new Exception(\"RECURSIVE: "+frame.getMethod+"\")")
      }

      budget -= 1
      
      handler(frame)
    }


    def execMethod(mframe: InterpreterFrame): Rep[Unit] = {
      import scala.collection.JavaConversions._

      // obtain block mapping that will tell us about dominance relations
      val method = mframe.getMethod()
      val graalBlocks = getGraalBlocks(method)
      def getGraalBlock(fr: InterpreterFrame) = graalBlocks.blocks.find(_.startBci == fr.getBCI).get

/*
      println("/*")
      val postDom = postDominators(graalBlock.blocks.toList)
      for (b <- graalBlock.blocks) {
          println(b + " succ [" + postDom(b).map("B"+_.blockID).mkString(",") + "]")
      }
      println("*/")
*/

      val saveHandler = handler
      val saveDepth = getContext(mframe).length

      if (debugMethods) println("// << " + method)

      case class BlockInfo(inEdges: List[(Int,State)], inState: State)
      case class BlockInfoOut(returns: List[State], gotos: List[State], code: String)

      val blockInfo: mutable.Map[Int, BlockInfo] = new mutable.HashMap
      val blockInfoOut: mutable.Map[Int, BlockInfoOut] = new mutable.HashMap

      var worklist: List[Int] = Nil

      var curBlock = -1

      // helpers

      def getState(frame: InterpreterFrame) = (freshFrameSimple(frame), store)
      def withState[A](state: State)(f: InterpreterFrame => A): A = { store = state._2; f(state._1) }


      // *** entry point: main control transfer handler ***
      handler = { blockFrame =>
        val d = getContext(blockFrame).length

        if (d > saveDepth) execMethod(blockFrame)
        else if (d < saveDepth) { 
          val s = getState(blockFrame)
          val out = blockInfoOut(curBlock)
          println("RETURN_"+curBlock+"_"+(out.returns.length)+";")
          blockInfoOut(curBlock) = out.copy(returns = out.returns :+ s)
          //returns = returns :+ (freshFrameSimple(blockFrame), store)
          //println("RETURN_"+(returns.length-1)+";")
          liftConst(())
        } else {
          gotoBlock(blockFrame)
        }
      }

      def gotoBlock(blockFrame: InterpreterFrame): Rep[Unit] = {
        // make sure we're still in the same method! --> catch external calls that don't reset handler
        assert(mframe.getMethod == blockFrame.getMethod, {"\n" +
                mframe.getMethod + "\n" +
                blockFrame.getMethod})

        val s = getState(blockFrame)
        val b = getGraalBlock(blockFrame)
        blockInfo.get(b.blockID) match {
          case Some(BlockInfo(edges,state)) => 
            // CAVEAT: can only have one edge per block pair (unchecked!)
            val edges2 = edges.filterNot(_._1 == curBlock) :+ (curBlock,s) //update with s at curBlock!
            // alternative: don't compute lub here, just test prev with new state per edge
            val (state2,_) = allLubs(edges2.map(_._2))
            blockInfo(b.blockID) = BlockInfo(edges2,state2)
            if (!worklist.contains(b.blockID) && statesDiffer(state,state2)) {
              worklist = (b.blockID::worklist).sorted
            }
            // if (state != state2) worklist += b.blockID
          case None => 
            blockInfo(b.blockID) = BlockInfo(List((curBlock,s)),s)
            worklist = (b.blockID::worklist).sorted
        }

        val out = blockInfoOut(curBlock)
        println("GOTO_"+(out.gotos.length)+";")
        blockInfoOut(curBlock) = out.copy(gotos = out.gotos :+ s)
        liftConst(())
      }


      // *** compute fixpoint ***

      val (src, res) = captureOutputResult {
        //gotoBlock(mframe) // alternative; just do it ourselves ...
        val s = getState(mframe)
        val b = getGraalBlock(mframe)
        blockInfo(b.blockID) = BlockInfo(List((-1,s)),s)
        worklist = List(b.blockID)

        while (worklist.nonEmpty) {
          val i = worklist.head
          worklist = worklist.tail
          assert(blockInfo.contains(i))
          curBlock = i
          blockInfoOut(i) = BlockInfoOut(Nil,Nil,"") // reset gotos
          val BlockInfo(edges,s) = blockInfo(i)
          val (src,_) = captureOutputResult {
            withState(s)(execFoReal)
          }
          blockInfoOut(i) = blockInfoOut(i).copy(code=src)
        }

        // reached fixpoint, now use acquired info to emit code

        def getPreds(i: Int) = blockInfo(i).inEdges.map(_._1)
        def shouldInline(i: Int) = getPreds(i).length < 2
        assert(getPreds(b.blockID) == List(-1))

        // fix jumps inside blocks: either call or inline
        for (b <- graalBlocks.blocks.reverse if blockInfo.contains(b.blockID)) {
          val i = b.blockID
          val BlockInfo(edges, stateBeforeBlock) = blockInfo(i)
          val BlockInfoOut(returns, gotos, code) = blockInfoOut(i)

          var src = code
          for ((s0,i) <- gotos.zipWithIndex) {
            val bid = getGraalBlock(s0._1).blockID
            val s1 = blockInfo(bid).inState
            val rhs = if (shouldInline(bid)) {
              assert(!statesDiffer(s0,s1))
              blockInfoOut(bid).code
            } else { // emit call
              val fields = getFields(s1)
              val (key,keyid) = contextKeyId(s1._1)
              val (_,_::head::Nil) = allLubs(List(s1,s0)) // could do just lub? yes, with captureOutput...
              ";{"+head.trim + "\nBLOCK_"+keyid+"("+fields.mkString(",")+")}"
            }
            src = src.replace("GOTO_"+i+";", rhs)
          }
          blockInfoOut(i) = BlockInfoOut(returns, gotos, src) // update body src
        }

        // initial block -- do we ever need to lub here?
        assert(getPreds(b.blockID) == List(-1))
        println(blockInfoOut(b.blockID).code)

        // emit all non-inlined blocks
        for (b <- graalBlocks.blocks if blockInfo.contains(b.blockID)) {
          val i = b.blockID
          if (!shouldInline(i)) {
            val BlockInfo(edges, stateBeforeBlock) = blockInfo(i)
            val BlockInfoOut(returns, gotos, code) = blockInfoOut(i)

            val fields = getFields(stateBeforeBlock)
            val (key,keyid) = contextKeyId(stateBeforeBlock._1)

            println("// "+key)
            println("def BLOCK_"+keyid+"("+fields.map(v=>v+":"+v.typ).mkString(",")+"): Unit = {")
            println(code)
            println("}")
          }
        }

        liftConst(())
      }

      // *** reset state

      handler = saveHandler

      // *** backpatch returns and continue ***

      if (debugMethods) println("// >> " + method)

      val returns = blockInfoOut.toList flatMap { case (i, out) =>
        out.returns.zipWithIndex.map { case (st, j) => ("RETURN_"+i+"_"+j+";",st) }
      }

      if (returns.length == 0) {
        print(src)
        println("// (no return?)")
      } else if (returns.length == 1) { 
        val (k,s) = returns(0)
        val (retSrc,res) = captureOutputResult {
          if (debugReturns) println("// ret single "+method)
          withState(s)(exec)
        }
        print(src.replace(k, retSrc.trim))
        res
      } else {
        println("// WARNING: multiple returns ("+returns.length+") in " + mframe.getMethod)

        val (ss, gos) = allLubs(returns.map(_._2))
        val fields = getFields(ss)

        println(";{")
        for (v <- fields) println("var v"+v+" = null.asInstanceOf["+v.typ+"]")

        var src1 = src
        for ((k,go) <- (returns.map(_._1)) zip gos) {
          val assign = fields.map { v => "v"+v+" = "+v }.mkString("\n")
          src1 = src1.replace(k,"/*R"+k.substring(6)+"*/;{" + go+assign+"};") // substr prevents further matches
        }
        print(src1)
        
        println(";{")
        if (debugReturns) println("// ret multi "+method)
        for (v <- fields) println("val "+v+" = v"+v)
        withState(ss)(exec)
        println("}}")
      }
      res
    }


    // halt on basic block boundaries: do not silently skip into the next block
    // (identify block start bci by looking at BasicBlockMapping)

    override def executeInstruction(frame: InterpreterFrame, bs: BytecodeStream): Control = {
      import collection.JavaConversions._
      val c = super.executeInstruction(frame,bs)

      val graalBlock = graalBlockMapping(frame.getMethod)

      val bci = bs.nextBCI()

      if (graalBlock.blocks.exists(_.startBci == bci)) {
        if (c == null) {
          //println("// *** silently going into next block from "+contextKey(frame))
          bs.next()
          local { (frame, bs) => 
            assert(bci == bs.currentBCI)
            exec(frame, bs.currentBCI) }
        } else c
      } else c
    }


    // actually execute a block's bytecode 
    def execFoReal(frame: InterpreterFrame): Rep[Unit] = {

      val key = contextKey(frame)
      val id = info.getOrElseUpdate(key, { val id = count; count += 1; id })

      if (debugStats) stats(key) = stats.getOrElse(key,0) + 1

      if (debugBlocks) println("// *** " + key)

      var saveStore = store

      val frame3 = freshFrameSimple(frame)
      val bci = frame3.getBCI()
      val bs = new BytecodeStream(frame3.getMethod.code())
      //bs.setBCI(globalFrame.getBCI())
      val res = try { executeBlock(frame3, bs, bci) } catch {
        case e: InterpreterException =>
          println("// caught " + e)
          reflect[Unit]("throw "+e.cause+".asInstanceOf[Throwable]")
        case e: Throwable =>
          println("ERROR /*")
          println(key)
          println(e.toString)
          e.getStackTrace().take(100).map(println)
          println("*/")
          liftConst(())
      }

      store = saveStore // need to reset?

      res
    }

    // not used -- we manage local worklists in execMethod
    def loop(root: InterpreterFrame, main: InterpreterFrame): Unit = {
      pushAsObjectInternal(root, main.getMethod.signature().returnKind(), Dyn[Object]("null /* stub return value "+main.getMethod.signature().returnKind()+" */")); // TODO: cleanup?
    }

    // print stats after compiling
    override def compile[A:Manifest,B:Manifest](f: A=>B): A=>B = {
      val f1 = try super.compile(f) finally if (debugStats) {
        println("--- stats ---")
        val stats1 = stats.toList.map { case (k,v) => 
          val frame = k.split("//").map { s => 
            val Array(bci,meth) = s.split(":")
            meth + ":" + bci
          }
          frame.reverse.mkString(" // ") + "    " + v
        }
        stats1.sorted foreach println
        println("total: " + stats.map(_._2).sum)
      }
      f1
    }

}