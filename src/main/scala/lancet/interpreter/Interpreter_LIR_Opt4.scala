package lancet.interpreter

import lancet.core._

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;

import com.oracle.graal.java.BciBlockMapping

import scala.collection.{mutable,immutable}

// (todo) cse --> elim redundant checks


class BytecodeInterpreter_LIR_Opt extends BytecodeInterpreter_LIR_Opt4


// version 4 -- reverse engineer more of the program block structure (if, loop)


trait AbstractInterpreter_LIR extends AbstractInterpreterIntf_LIR with BytecodeInterpreter_LIR with RuntimeUniverse_LIR_Opt {

    // hack: base_opt doesn't have access to runtime
    override def getFieldForLub[T:TypeRep](base: Rep[Object], cls: Class[_], k: String): Rep[T] = {
      val fld = metaAccessProvider.lookupJavaType(cls).getInstanceFields(false).find(_.getName == k)
      fld.map(f => runtimeInterface.asInstanceOf[Runtime_Opt].getFieldConst[T](base,f)).
        getOrElse(getFieldForLub[T](base,cls.getSuperclass,k))
    }


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



    // TODO: externalize
    // side-effect: may create definition phi_str = b
    def phi(str: String, a: Rep[Object], b: Rep[Object]) = if (a == b) b else {
      if (b == null)
        emitString("val "+str+" = null.asInstanceOf["+a.typ+"] // LUBC(" + a + "," + b + ")") // FIXME: kill in expr!
      else if (b.toString != str)
        emitString("val "+str+" = " + b + " // LUBC(" + (if(a==null)a else a + ":"+a.typ)+"," + b + ":"+b.typ+ ")") // FIXME: kill in expr!
      val tp = (if (b == null) a.typ else b.typ).asInstanceOf[TypeRep[AnyRef]] // NPE? should take a.typ in general?
      Dyn[AnyRef](str)(tp)
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
          val str = "PHI_"+x.depth+"_"+i
          y.locals(i) = phi(str,a,b)
        }

        lub(x.getParentFrame, y.getParentFrame)
        y
      }
    }

    // calc lubs and backpatch info for jumps
    type State = (InterpreterFrame, StoreLattice.Elem, ExprLattice.Elem)

    def allLubs(states: List[State]): (State,List[Block[Unit]]) = {
      if (states.length == 1) return (states.head, List(Block(Nil,liftConst(())))) // fast path
      // backpatch info: foreach state, commands needed to initialize lub vars
      val gos = states map { case (frameX,storeX,exprX) =>
        val frameY = freshFrameSimple(frameX)
        var storeY = storeX
        var exprY = exprX
        val go = reify { // start with 'this' state, make it match all others
          states foreach { case (f,s,e) => 
            FrameLattice.lub(f, frameY) 
            storeY = StoreLattice.lub(s,storeY)
            exprY = ExprLattice.lub(e,exprY)
          }
          //val locals = FrameLattice.getFields(frameY).filter(_.toString.startsWith("PHI"))
          //val fields = StoreLattice.getFields(storeY).filter(_.toString.startsWith("LUB"))
          //for (v <- locals ++ fields) emitString("v"+v+" = "+v)
          liftConst(())
        }
        (go,frameY,storeY,exprY)
      }
      val (_,f02,s02,e02) = gos(0)
      for ((_,fx,sx,ex) <- gos) { // sanity check
        assert(contextKey(f02) == contextKey(fx))
        assert(getAllArgs(f02) == getAllArgs(fx))
        assert(s02 == sx, s02+"!=="+sx)
        assert(e02 == ex, e02+"!=="+ex)
      }
      ((f02,s02,e02),gos.map(_._1))
    }

    def getFrame(s: State) = s._1

    def getFields(s: State) = {
      val locals = FrameLattice.getFields(s._1).filterNot(_.isInstanceOf[Static[_]])//filter(_.toString.startsWith("PHI"))
      val fields = StoreLattice.getFields(s._2).filterNot(_.isInstanceOf[Static[_]])//.filter(_.toString.startsWith("LUB"))
      val exprs = ExprLattice.getFields(s._3).filterNot(_.isInstanceOf[Static[_]])//.filter(_.toString.startsWith("LUB"))
      (locals ++ fields ++ exprs).distinct.sortBy(_.toString)
    }

    def statesDiffer(s0: State, s1: State) = 
      getAllArgs(s0._1) != getAllArgs(s1._1) || s0._2 != s1._2 || s0._3 != s1._3

    def freshFrameSimple(frame: InterpreterFrame): InterpreterFrame_Str = if (frame eq null) null else {
      val frame2 = frame.asInstanceOf[InterpreterFrame_Str].copy2(freshFrameSimple(frame.getParentFrame))
      val depth = frame2.depth
      frame2
    }


    def getAllArgs(frame: InterpreterFrame) = frame.getReturnValue()::getContext(frame).dropRight(1).flatMap(_.asInstanceOf[InterpreterFrame_Str].locals)

    def getState(frame: InterpreterFrame) = (freshFrameSimple(frame), store, exprs)
    def withState[A](state: State)(f: InterpreterFrame => A): A = { store = state._2; exprs = state._3; f(state._1) }

    def getState0 = (null, store, exprs) // TODO: cleanup
    def setState0(state: State): Unit = { store = state._2; exprs = state._3 }


}



trait AbstractInterpreterIntf_LIR extends BytecodeInterpreter_LIR with Core_LIR {

    type State

    def getFrame(s: State): InterpreterFrame
    def getFields(s: State): List[Rep[Any]]

    def allLubs(states: List[State]): (State,List[Block[Unit]])
    def statesDiffer(s0: State, s1: State): Boolean

    def freshFrameSimple(frame: InterpreterFrame): InterpreterFrame_Str

    def getState(frame: InterpreterFrame): State
    def withState[A](state: State)(f: InterpreterFrame => A): A

    def getState0: State
    def setState0(s: State): Unit

}





class BytecodeInterpreter_LIR_Opt4 extends AbstractInterpreter_LIR with BytecodeInterpreter_LIR_Opt4Engine with BytecodeInterpreter_LIR with RuntimeUniverse_LIR_Opt {
    override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m)

    override def withScope[A](body: =>A): A = { // TODO: put somewhere else
      val save = getState0
      try {
        super.withScope(body)
      } finally {
        setState0(save)
      }
    }

    /*def genGoto(key: String) = {
      reflect[Unit](Patch(key, Block(Const(()))))
    }
    def genGotoDef(key: String, rhs: Block[Unit]) = {
      var hit = false
      globalDefs.foreach {
        case d@TP(s,Reflect(g @ Patch(`key`, _), u, es)) => 
          //println("FOUND "+d);Patch          
          hit = true
          g.block = rhs
        case d => d
      }
      assert(hit)
    }*/


    def genBlockCall(keyid: Int, fields: List[Rep[Any]]) = "BLOCK_"+keyid+"("+fields.mkString(",")+")"

    def genBlockDef(key: String, keyid: Int, fields: List[Rep[Any]], code: Block[Unit]): Block[Unit] = reify {
      if (debugBlockKeys) emitString("// "+key+"\n")
      reflect[Unit]("def BLOCK_"+keyid+"("+fields.map(v=>v+":"+v.typ).mkString(",")+"): Unit = ", code)
    }


    def genVarDef(v: Rep[Any]): String = "var v"+v+" = null.asInstanceOf["+v.typ+"]"
    def genVarWrite(v: Rep[Any]): String = "v"+v+" = "+v
    def genVarRead(v: Rep[Any]): String = "val "+v+" = v"+v


    // not used -- we manage local worklists in execMethod
    def loop(root: InterpreterFrame, main: InterpreterFrame): Unit = {
      pushAsObjectInternal(root, main.getMethod.getSignature().getReturnKind(), Dyn[Object]("null /* stub return value "+main.getMethod.getSignature().getReturnKind()+" */")); // TODO: cleanup?
    }

    // print stats after compiling
    override def compile[A:Manifest,B:Manifest](f: A=>B): A=>B = {
      val f1 = try super.compile(f) finally if (debugStats) {
        emitString("--- stats ---")
        val stats1 = stats.toList.map { case (k,v) => 
          val frame = k.split("//").map { s => 
            val Array(bci,meth) = s.split(":")
            meth + ":" + bci
          }
          frame.reverse.mkString(" // ") + "    " + v
        }
        stats1.sorted foreach emitString
        emitString("total: " + stats.map(_._2).sum)
      }
      f1
    }


}


trait BytecodeInterpreter_LIR_Opt4Engine extends AbstractInterpreterIntf_LIR with BytecodeInterpreter_LIR with Core_LIR {

    // config options

    var debugBlocks = false
    var debugBlockKeys = true
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
    def getGraalBlocks(method: ResolvedJavaMethod, bci: Int) = if (bci == 0) graalBlockMapping.getOrElseUpdate(method, {
      val map = new BciBlockMapping(method);
      map.build();
      map
    }) else { // OSR
      assert(bci >= 0,"bci: "+bci)
      import scala.collection.JavaConversions._
      val map = new BciBlockMapping(method);
      map.build();
      emitString("// need to fix block ordering for bci="+bci)
      emitString("// old: " + map.blocks.mkString(","))

      val start = map.blocks.find(_.startBci == bci).get
      var reach = List[BciBlockMapping.Block]()
      var seen = Set[BciBlockMapping.Block]()
      def rec(block: BciBlockMapping.Block) {
        if (!seen(block)) {
          seen += block
          block.successors.foreach(rec)
          reach = block::reach
        }
      }
      rec(start)
      emitString("// new: " + reach.mkString(","))
      map.blocks.clear
      map.blocks.addAll(reach)
      // do we really need to rename them??
      for ((b,i) <- reach.zipWithIndex) b.blockID = i
      emitString("// fixed: " + map.blocks.mkString(","))
      map
    }

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
      try {
        handler = execMethod
        depth = 0
        body
      } finally {
        handler = saveHandler
        depth = saveDepth
      }
    }

    // abstract methods

    def genBlockCall(keyid: Int, fields: List[Rep[Any]]): String
    def genBlockDef(key: String, keyid: Int, fields: List[Rep[Any]], code: Block[Unit]): Block[Unit]
    def genVarDef(v: Rep[Any]): String
    def genVarWrite(v: Rep[Any]): String
    def genVarRead(v: Rep[Any]): String

    def emitAll(b: Block[Unit]) = b.stms foreach emit

    // TODO: proper subst transformer

    def infix_replace[A,B](a: Block[A], key: String, b: Block[B]): Block[A] = {
      var found = 0
      val t = new StructuralTransformer {
        override def transformStm(s: Stm): List[Stm] = s match {
          case Unstructured(`key`) => 
            found += 1
            // not replacing a valdef so we can discard result
            b.stms
          case Unstructured(k) => 
            //System.out.println("no match "+k)
            super.transformStm(s)
          case _ => super.transformStm(s)
        }
      }
      val r = t.transformBlock(a)
      if (found == 0) System.out.println("not found: "+key)
      r
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
        emitString("// *** BUDGET EXCEEDED ***")
        return unit(().asInstanceOf[Object]).asInstanceOf[Rep[Unit]]
      }

      if (frame.getParentFrame == null) { // TODO: cleanup?
        val p = popAsObject(frame, frame.getMethod.getSignature.getReturnKind())
        return reflect[Unit]("(RES = "+p+") // return to root")
      }

      val method = frame.getMethod()
      if (!emitRecursive && getContext(frame).drop(1).exists(_.getMethod() == method)) { // recursive (TODO: faster test)
        emitString("// *** RECURSIVE: "+method+" *** " + contextKey(frame))
        return reflect[Unit]("throw new Exception(\"RECURSIVE: "+frame.getMethod+"\")")
      }

      budget -= 1
      
      handler(frame)
    }


    def execMethod(mframe: InterpreterFrame): Rep[Unit] = {
      import scala.collection.JavaConversions._

      // obtain block mapping that will tell us about dominance relations
      val method = mframe.getMethod()
      val graalBlocks = getGraalBlocks(method,mframe.getBCI)
      def getGraalBlock(fr: InterpreterFrame) = graalBlocks.blocks.find(_.startBci == fr.getBCI).get

/*
      emitString("/*")
      val postDom = postDominators(graalBlock.blocks.toList)
      for (b <- graalBlock.blocks) {
          emitString(b + " succ [" + postDom(b).map("B"+_.blockID).mkString(",") + "]")
      }
      emitString("*/")
*/

      val saveHandler = handler
      val saveDepth = getContext(mframe).length

      if (debugMethods) emitString("// << " + method)
      
      val (mkey,mkeyid) = contextKeyId(mframe)

      case class BlockInfo(inEdges: List[(Int,State)], inState: State)
      case class BlockInfoOut(returns: List[State], gotos: List[State], code: Block[Unit])

      val blockInfo: mutable.Map[Int, BlockInfo] = new mutable.HashMap
      val blockInfoOut: mutable.Map[Int, BlockInfoOut] = new mutable.HashMap

      var worklist: List[Int] = Nil

      var curBlock = -1

      // *** entry point: main control transfer handler ***
      handler = { blockFrame =>
        val d = getContext(blockFrame).length

        //emitString("// ctrl transfer "+d+" "+contextKey(blockFrame))

        if (d > saveDepth) execMethod(blockFrame)
        else if (d < saveDepth) { 
          val s = getState(blockFrame)
          val out = blockInfoOut(curBlock)
          if (debugReturns) emitString("// return "+curBlock+"_"+(out.returns.length)+" to "+contextKey(blockFrame))
          emitString("RETURN_"+mkeyid+"_"+curBlock+"_"+(out.returns.length)+";")
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
        emitString("GOTO_"+mkeyid+"_"+curBlock+"_"+(out.gotos.length))
        blockInfoOut(curBlock) = out.copy(gotos = out.gotos :+ s)
        liftConst(())
      }


      // *** compute fixpoint ***

      val block = reify {
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
          blockInfoOut(i) = BlockInfoOut(Nil,Nil,Block(Nil,liftConst(()))) // reset gotos
          val BlockInfo(edges,s) = blockInfo(i)
          val src = reify {
            withState(s)(execFoReal)
          }
          blockInfoOut(i) = blockInfoOut(i).copy(code=src)
        }

        // reached fixpoint, now use acquired info to emit code

        def getPreds(i: Int) = blockInfo(i).inEdges.map(_._1)
        def shouldInline(i: Int) = getPreds(i).length < 2
        //assert(getPreds(b.blockID) == List(-1)) we do have preds for OSR!

        // fix jumps inside blocks: either call or inline
        for (b <- graalBlocks.blocks.reverse if blockInfo.contains(b.blockID)) {
          val i = b.blockID
          val BlockInfo(edges, stateBeforeBlock) = blockInfo(i)
          val BlockInfoOut(returns, gotos, code) = blockInfoOut(i)

          var src = code
          for ((s0,i) <- gotos.zipWithIndex) {
            val bid = getGraalBlock(getFrame(s0)).blockID
            val s1 = blockInfo(bid).inState
            val rhs = if (shouldInline(bid)) {
              assert(!statesDiffer(s0,s1))
              blockInfoOut(bid).code
            } else { // emit call
              val fields = getFields(s1)
              val (key,keyid) = contextKeyId(getFrame(s1))
              val (_,_::head::Nil) = allLubs(List(s1,s0)) // could do just lub? yes, with captureOutput...
              val call = genBlockCall(keyid, fields)
              reify { reflect[Unit](";", Block[Unit](head.stms:+Unstructured(call), head.res)) }
            }
            src = src.replace("GOTO_"+mkeyid+"_"+b.blockID+"_"+i, rhs)
          }
          blockInfoOut(i) = BlockInfoOut(returns, gotos, src) // update body src
        }

        // initial block -- do we ever need to lub here?
        //assert(getPreds(b.blockID) == List(-1)) we do have preds for OSR!
        //emitAll(blockInfoOut(b.blockID).code)

        if (shouldInline(b.blockID)) {
          emitAll(blockInfoOut(b.blockID).code)
        } else {
          emitString("// should not inline start block "+b.blockID)
          val s0 = blockInfo(b.blockID).inEdges.find(_._1 == -1).get._2
          val s1 = blockInfo(b.blockID).inState
          val fields = getFields(s1)
          val (key,keyid) = contextKeyId(getFrame(s1))
          val (_,_::head::Nil) = allLubs(List(s1,s0)) // could do just lub? yes, with captureOutput...
          val call = genBlockCall(keyid, fields)
          reflect[Unit](";", Block[Unit](head.stms:+Unstructured(call), head.res))
        }

        // emit all non-inlined blocks
        for (b <- graalBlocks.blocks if blockInfo.contains(b.blockID)) {
          val i = b.blockID
          if (!shouldInline(i)) {
            val BlockInfo(edges, stateBeforeBlock) = blockInfo(i)
            val BlockInfoOut(returns, gotos, code) = blockInfoOut(i)

            val fields = getFields(stateBeforeBlock)
            val (key,keyid) = contextKeyId(getFrame(stateBeforeBlock))

            emitAll(genBlockDef(key, keyid, fields, code))
          }
        }

        liftConst(())
      }

      // *** reset state

      handler = saveHandler

      // *** backpatch returns and continue ***

      val returns = blockInfoOut.toList flatMap { case (i, out) =>
        out.returns.zipWithIndex.map { case (st, j) => ("RETURN_"+mkeyid+"_"+i+"_"+j+";",st) }
      }

      // need to split between different return targets!!!
      // do we need to consider more cases than just discarding stuff?

      val retframes = returns.groupBy(r => contextKey(getFrame(r._2)))

      if (returns.length == 0) {
        emitAll(block)
        emitString("// (no return?)")
      } else if (returns.length == 1) { // crash test3 if disabled?
        val (k,s) = returns(0)
        val ret = reify {
          if (debugReturns) emitString("// ret single "+method)
          if (debugMethods) emitString("// >> " + method)
          withState(s)(exec)
        }
        emitAll(block.replace(k, ret))
      } else {
        emitString("// WARNING: multiple returns ("+returns.length+") in " + mframe.getMethod)

        if (retframes.size == 1) { // just 1 target
          val (ss, gos) = allLubs(returns.map(_._2))
          val fields = getFields(ss)

          emitString(";{")

          for (v <- fields) emitString(genVarDef(v))

          var block1 = block
          for ((k,go) <- (returns.map(_._1)) zip gos) {
            val assign = Block(fields.map(genVarWrite).map(Unstructured(_)), liftConst(()))
            val ret = reify {
              reflect[Unit]("/*R"+k.substring(6)+"*/;", Block(go.stms++assign.stms,assign.res),";") // substr prevents further matches            
            }
            block1 = block1.replace(k,ret)
          }
          emitAll(block1)
          
          emitString(";{")
          if (debugReturns) emitString("// ret multi "+method)
          for (v <- fields) emitString(genVarRead(v))

          if (debugMethods) emitString("// >> " + method)
          withState(ss)(exec)
          emitString("}}")
        } else {
          // multiple targets!
          var block1 = block
          for ((target, returns) <- retframes) {
            assert(returns.length == 1) // not handling multiple calls to multiple targets...
            val (k,s) = returns(0)
            val ret = reify {
              if (debugMethods) emitString("// >> " + method)
              withState(s)(exec)
            }
            block1 = block1.replace(k, ret)
          }
          emitAll(block1)
        }
      }
      liftConst(())
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

      val (key,id) = contextKeyId(frame)

      if (debugStats) stats(key) = stats.getOrElse(key,0) + 1

      if (debugBlocks) emitString("// *** " + key)

      val frame1 = freshFrameSimple(frame) // necessary?
      val bci = frame1.getBCI()
      val bs = new BytecodeStream(frame1.getMethod.getCode())
      //bs.setBCI(globalFrame.getBCI())
      val res = try { executeBlock(frame1, bs, bci) } catch {
        case e: InterpreterException =>
          emitString("// caught " + e)
          reflect[Unit]("throw "+e.cause+".asInstanceOf[Throwable]")
        case e: Throwable =>
          val e1 = e match {
            case e: java.lang.reflect.InvocationTargetException => e.getCause
            case _ => e
          }
          emitString("ERROR /*")
          emitString(key)
          emitString(e1.toString)
          emitString(e1.getStackTrace().take(100).mkString("\n"))
          emitString("*/")
          liftConst(())
      }

      res
    }


}