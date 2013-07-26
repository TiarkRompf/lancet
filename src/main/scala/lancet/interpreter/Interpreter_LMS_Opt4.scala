/*
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/agpl.html.
 * 
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package lancet.interpreter

import lancet.core._

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;

import com.oracle.graal.java.BciBlockMapping

import scala.collection.{mutable,immutable}

// (todo) cse --> elim redundant checks


class BytecodeInterpreter_LMS_Opt extends BytecodeInterpreter_LMS_Opt4


// version 4 -- reverse engineer more of the program block structure (if, loop)


trait AbstractInterpreter_LMS extends AbstractInterpreterIntf_LMS with BytecodeInterpreter_LMS with RuntimeUniverse_LMS_Opt {
  import IR._

    // hack: base_opt doesn't have access to runtime
    override def getFieldForLub[T:TypeRep](base: Rep[Object], cls: Class[_], k: String): Rep[T] = {
      val fld = metaAccessProvider.lookupJavaType(cls).getInstanceFields(false).find(_.getName == k)
      fld.map(f => runtimeInterface.asInstanceOf[Runtime_Opt].getFieldConst[T](base,f)).
        getOrElse(getFieldForLub[T](base,cls.getSuperclass,k))
    }


    override def objectGetClass(receiver: Rep[Object]): Option[Class[_]] = {
      eval(receiver) match {
        case Partial(fs) if fs.contains("clazz") => 
          val VConst(clazz: Class[_]) = eval(fs("clazz"))
          Some(clazz)
        case Partial(fs) => 
          val Static(x: NotNull) = fs("alloc") // unsafe? <-- could also set "clazz" field when lifting VConst
          Some(x.getClass)
        case VConst(x: NotNull) =>
          val clazz = x.getClass
          Some(clazz)
        case _ =>
          None
      }        
    }


    def contextKeyId(frame: InterpreterFrame): (String, Int)

    
    // the current block id. set by allLubs, and picked up by phi/lub operations
    var curBlockId = -1 // FIXME: make less ad-hoc


    // TODO: externalize
    // side-effect: may create definition phi_str = b
    def phi(str: String, a: Rep[Object], b: Rep[Object]) = if (a == b) b else {
      if (b == null)
        reflect[Unit]("val "+str+" = null.asInstanceOf["+a.typ+"] // LUBC(" + a + "," + b + ")") // FIXME: kill in expr!
      else if (quote(b) != str)
        reflect[Unit]("val "+str+" = ",b," // LUBC(" + (if(a==null)a else a + ":"+a.typ)+"," + b + ":"+b.typ+ ")") // FIXME: kill in expr!
      val tp = (if (b == null) a.typ else b.typ).asInstanceOf[TypeRep[AnyRef]] // NPE? should take a.typ in general?
      val res = Dyn[AnyRef](str)(tp)
      res.keyid = curBlockId
      res
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
          val str = "PHI_b"+curBlockId+"_"+x.depth+"_"+i
          y.locals(i) = phi(str,a,b)
        }

        lub(x.getParentFrame, y.getParentFrame)
        y
      }
    }

    // calc lubs and backpatch info for jumps
    type IState = (InterpreterFrame, StoreLattice.Elem, ExprLattice.Elem, CondLattice.Elem)

    def allLubs(states: List[IState]): (IState,List[Block[Unit]]) = {
      if (states.length == 1) return (states.head, List(reify[Unit](liftConst(())))) // fast path
      // backpatch info: foreach state, commands needed to initialize lub vars
      val gos = states map { case (frameX,storeX,exprX,condX) =>
        val frameY = freshFrameSimple(frameX)
        var storeY = storeX
        var exprY = exprX
        var condY = condX
        val go = reify { // start with 'this' state, make it match all others
          states foreach { case (f,s,e,c) => 
            val (mkey, mkeyId) = contextKeyId(f)
            curBlockId = mkeyId
            FrameLattice.lub(f, frameY) 
            storeY = StoreLattice.lub(s,storeY)
            exprY = ExprLattice.lub(e,exprY)
            condY = CondLattice.lub(c,condY)
          }
          //val locals = FrameLattice.getFields(frameY).filter(_.toString.startsWith("PHI"))
          //val fields = StoreLattice.getFields(storeY).filter(_.toString.startsWith("LUB"))
          //for (v <- locals ++ fields) emitString("v"+v+" = "+v)
          liftConst(())
        }
        (go,frameY,storeY,exprY,condY)
      }
      val (_,f02,s02,e02,c02) = gos(0)
      for ((_,fx,sx,ex,cx) <- gos) { // sanity check
        assert(contextKey(f02) == contextKey(fx))
        assert(getAllArgs(f02) == getAllArgs(fx))
        assert(s02 == sx, s02+"!=="+sx)
        assert(e02 == ex, e02+"!=="+ex)
      }
      ((f02,s02,e02,c02),gos.map(_._1))
    }

    def getFrame(s: IState) = s._1

    def getFields(s: IState) = {
      val locals = FrameLattice.getFields(s._1).filterNot(_.isInstanceOf[Static[_]])//filter(_.toString.startsWith("PHI"))
      val fields = StoreLattice.getFields(s._2).filterNot(_.isInstanceOf[Static[_]])//.filter(_.toString.startsWith("LUB"))
      val exprs = ExprLattice.getFields(s._3).filterNot(_.isInstanceOf[Static[_]])//.filter(_.toString.startsWith("LUB"))
      (locals ++ fields ++ exprs).distinct.sortBy(_.toString)
    }

    def statesDiffer(s0: IState, s1: IState) = 
      getAllArgs(s0._1) != getAllArgs(s1._1) || s0._2 != s1._2 || s0._3 != s1._3

    def freshFrameSimple(frame: InterpreterFrame): InterpreterFrame_Str = if (frame eq null) null else {
      val frame2 = frame.asInstanceOf[InterpreterFrame_Str].copy2(freshFrameSimple(frame.getParentFrame))
      val depth = frame2.depth
      frame2
    }

    var exprs: ExprLattice.Elem = ExprLattice.bottom // move somewhere else?

    def getAllArgs(frame: InterpreterFrame) = frame.getReturnValue()::getContext(frame).dropRight(1).flatMap(_.asInstanceOf[InterpreterFrame_Str].locals)

    def getState(frame: InterpreterFrame) = (freshFrameSimple(frame), store, exprs, conds)
    def withState[A](state: IState)(f: InterpreterFrame => A): A = { store = state._2; exprs = state._3; ; conds = state._4; f(state._1) }

    def getState0 = (null, store, exprs, conds) // TODO: cleanup
    def setState0(state: IState): Unit = { store = state._2; exprs = state._3; conds = state._4 }


}



trait AbstractInterpreterIntf_LMS extends BytecodeInterpreter_LMS with Core_LMS {

    type IState

    def getFrame(s: IState): InterpreterFrame
    def getFields(s: IState): List[Rep[Any]]

    def allLubs(states: List[IState]): (IState,List[Block[Unit]])
    def statesDiffer(s0: IState, s1: IState): Boolean

    def freshFrameSimple(frame: InterpreterFrame): InterpreterFrame_Str

    def getState(frame: InterpreterFrame): IState
    def withState[A](state: IState)(f: InterpreterFrame => A): A

    def getState0: IState
    def setState0(s: IState): Unit

}





class BytecodeInterpreter_LMS_Opt4 extends AbstractInterpreter_LMS with BytecodeInterpreter_LMS_Opt4Engine with BytecodeInterpreter_LMS with RuntimeUniverse_LMS_Opt {
    override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m)

    override def withScope[A](body: =>A): A = { // TODO: put somewhere else
      val save = getState0
      try {
        super.withScope(body)
      } finally {
        setState0(save)
      }
    }


    def genGoto(key: String) = {
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
    }

    def genBlockCall(keyid: Int, fields: List[Rep[Any]]) = 
      reflect[Unit]("BLOCK_"+keyid+"("+fields.map(quote).mkString(",")+")")

    def genBlockDef(key: String, keyid: Int, fields: List[Rep[Any]], code: Block[Unit]) =
      reflect(BlockDef(key,keyid,fields,code))


    def genVarDef(v: Rep[Any]): Rep[Unit] = reflect[Unit]("var v"+quote(v)+" = null.asInstanceOf["+v.typ+"]")
    def genVarWrite(v: Rep[Any]): Rep[Unit] = reflect[Unit]("v"+quote(v)+" = "+quote(v))
    def genVarRead(v: Rep[Any]): Rep[Unit] = reflect[Unit]("val "+quote(v)+" = v"+quote(v))


    // not used -- we manage local worklists in execMethod
    def loop(root: InterpreterFrame, main: InterpreterFrame): Unit = {
      pushAsObjectInternal(root, main.getMethod.getSignature().getReturnKind(), Dyn[Object]("null /* stub return value "+main.getMethod.getSignature().getReturnKind()+" */")); // TODO: cleanup?
    }

    // print stats after compiling
    /*override def compile[A:Manifest,B:Manifest](f: A=>B): A=>B = {
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
    }*/


}


trait BytecodeInterpreter_LMS_Opt4Engine extends AbstractInterpreterIntf_LMS with BytecodeInterpreter_LMS with Core_LMS {

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

    def getGraalBlocks(method: ResolvedJavaMethod, bci: Int) = 
    if (bci == 0) graalBlockMapping.getOrElseUpdate(method, {
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

    def defaultReturnHandler: (Rep[Object] => Rep[Unit]) = { p =>
      reflect[Unit]("("+RES+" = ",quote(p),") // return to root")
    }

    def defaultHandler: (InterpreterFrame => Rep[Unit]) = execMethod

    var handler: (InterpreterFrame => Rep[Unit]) = defaultHandler
    var returnHandler: (Rep[Object] => Rep[Unit]) = defaultReturnHandler
    var depth = 0

    def withScope[A](body: =>A): A = { // reset scope, e.g. for nested calls
      val saveHandler = handler
      val saveReturnHandler = returnHandler
      val saveDepth = depth
      val saveResId = curResId
      try {
        handler = defaultHandler
        returnHandler = defaultReturnHandler
        depth = 0
        curResId += 1 //still needed?
        body
      } finally {
        handler = saveHandler
        returnHandler = saveReturnHandler
        depth = saveDepth
        curResId = saveResId
      }
    }

    // abstract methods

    def genGoto(key: String): Rep[Unit]
    def genGotoDef(key: String, rhs: Block[Unit]): Rep[Unit]
    def genBlockCall(keyid: Int, fields: List[Rep[Any]]): Rep[Unit]
    def genBlockDef(key: String, keyid: Int, fields: List[Rep[Any]], code: Block[Unit]): Exp[Unit]
    def genVarDef(v: Rep[Any]): Rep[Unit]
    def genVarWrite(v: Rep[Any]): Rep[Unit]
    def genVarRead(v: Rep[Any]): Rep[Unit]



    // exec loop

    def exec(frame: InterpreterFrame): Rep[Unit] = { // called internally to initiate control transfer
      
      if (budget <= 0) {
        emitString("// *** BUDGET EXCEEDED ***")
        return unit(().asInstanceOf[Object]).asInstanceOf[Rep[Unit]]
      }

      if (frame.getParentFrame == null) { // TODO: cleanup?
        val p = popAsObject(frame, frame.getMethod.getSignature.getReturnKind())
        return returnHandler(p)
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

      case class BlockInfo(inEdges: List[(Int,IState)], inState: IState)
      case class BlockInfoOut(returns: List[IState], gotos: List[IState], code: Block[Unit])

      val blockInfo: mutable.Map[Int, BlockInfo] = new mutable.HashMap
      val blockInfoOut: mutable.Map[Int, BlockInfoOut] = new mutable.HashMap

      var worklist: List[Int] = Nil

      var curBlock = -1

      // *** entry point: main control transfer handler ***
      handler = { blockFrame =>
        val d = getContext(blockFrame).length

        if (d > saveDepth) execMethod(blockFrame)
        else if (d < saveDepth) { 
          val s = getState(blockFrame)
          val out = blockInfoOut(curBlock)
          if (debugReturns) emitString("// return "+curBlock+"_"+(out.returns.length)+" to "+contextKey(blockFrame))
          genGoto("RETURN_"+mkeyid+"_"+curBlock+"_"+(out.returns.length))
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
        genGoto("GOTO_"+mkeyid+"_"+curBlock+"_"+(out.gotos.length))
        blockInfoOut(curBlock) = out.copy(gotos = out.gotos :+ s)
        liftConst(())
      }


      // *** compute fixpoint ***
      
      genGoto("HEAD_"+mkeyid)

      //val block = reify {
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
        blockInfoOut(i) = BlockInfoOut(Nil,Nil,Block(liftConst(()))) // reset gotos
        val BlockInfo(edges,s) = blockInfo(i)
        val src = reify {
          withState(s)(execFoReal)
        }
        blockInfoOut(i) = blockInfoOut(i).copy(code=src)
      }

      // reached fixpoint, now use acquired info to emit code

      def getPreds(i: Int) = blockInfo(i).inEdges.map(_._1)
      def shouldInline(i: Int) = getPreds(i).length < 2
      //assert(getPreds(b.blockID) == List(-1))

      // fix jumps inside blocks: either call or inline
      for (b <- graalBlocks.blocks.reverse if blockInfo.contains(b.blockID)) {
        val i = b.blockID
        val BlockInfo(edges, stateBeforeBlock) = blockInfo(i)
        val BlockInfoOut(returns, gotos, code) = blockInfoOut(i)

        for ((s0,i) <- gotos.zipWithIndex) {
          val bid = getGraalBlock(getFrame(s0)).blockID
          val s1 = blockInfo(bid).inState
          val rhs = if (shouldInline(bid)) {
            assert(!statesDiffer(s0,s1))
            // TODO: when inlining in multiple places states may differ; do lub assignments
            blockInfoOut(bid).code
          } else { // emit call
            val fields = getFields(s1)
            val (key,keyid) = contextKeyId(getFrame(s1))
            val (_,_::head::Nil) = allLubs(List(s1,s0)) // could do just lub? yes, with captureOutput...
            reify { 
              emitString(";{")
              reflect[Unit](Patch("",head))
              genBlockCall(keyid, fields) 
              emitString("}")
            }
          }
          genGotoDef("GOTO_"+mkeyid+"_"+b.blockID+"_"+i, rhs)
          //src = src.replace("GOTO_"+i+";", rhs)
        }
        //blockInfoOut(i) = BlockInfoOut(returns, gotos, code) // update body src
      }

      // initial block -- do we ever need to lub here?
      //assert(getPreds(b.blockID) == List(-1))
      //reflect(Patch("",blockInfoOut(b.blockID).code))

      if (shouldInline(b.blockID)) {
        reflect(Patch("",blockInfoOut(b.blockID).code))
      } else {
        emitString("// should not inline start block "+b.blockID)
        val s0 = blockInfo(b.blockID).inEdges.find(_._1 == -1).get._2
        val s1 = blockInfo(b.blockID).inState
        val fields = getFields(s1)
        val (key,keyid) = contextKeyId(getFrame(s1))
        val (_,_::head::Nil) = allLubs(List(s1,s0)) // could do just lub? yes, with captureOutput...
        reflect[Unit](Patch("",head))
        genBlockCall(keyid, fields)
      }

      // emit all non-inlined blocks
      for (b <- graalBlocks.blocks if blockInfo.contains(b.blockID)) {
        val i = b.blockID
        if (!shouldInline(i)) {
          val BlockInfo(edges, stateBeforeBlock) = blockInfo(i)
          val BlockInfoOut(returns, gotos, code) = blockInfoOut(i)

          val fields = getFields(stateBeforeBlock)
          val (key,keyid) = contextKeyId(getFrame(stateBeforeBlock))

          genBlockDef(key, keyid, fields, code)
        }
      }
      //liftConst(())
      //}

      // *** reset state

      handler = saveHandler

      // *** backpatch returns and continue ***

      val returns = blockInfoOut.toList flatMap { case (i, out) =>
        out.returns.zipWithIndex.map { case (st, j) => ("RETURN_"+mkeyid+"_"+i+"_"+j,st) }
      }

      // need to split between different return targets!!!
      // do we need to consider more cases than just discarding stuff?

      val retframes = returns.groupBy(r => contextKey(getFrame(r._2)))

      if (returns.length == 0) {
        emitString("// (no return?)")
      } else if (returns.length == 1) {  // TODO
        val (k,s) = returns(0)
        val ret = reify {
          if (debugReturns) emitString("// ret single "+method)
          if (debugMethods) emitString("// >> " + method)
          withState(s)(exec)
        }
        genGotoDef(k, ret)
      } else {
        emitString("// WARNING: multiple returns ("+returns.length+") in " + mframe.getMethod)

        if (retframes.size == 1) { // just 1 target
          val (ss, gos) = allLubs(returns.map(_._2))
          val fields = getFields(ss)

          emitString(";{")

          val head = reify { for (v <- fields) { var w = genVarDef(v); emitString("// "+w) } }
          genGotoDef("HEAD_"+mkeyid, head)

          for ((k,go) <- (returns.map(_._1)) zip gos) {
            val block = reify[Unit]{ reflect[Unit](Patch("",go)); fields.map(genVarWrite); liftConst(()) }
            genGotoDef(k, block)
            //reflect[Unit]("def "+k.substring(6)+": Unit = {", go, assign,"};") // substr prevents further matches
          }
          
          // TODO: should emitAll(block) here!

          emitString(";{")
          if (debugReturns) emitString("// ret multi "+method)

          for (v <- fields) genVarRead(v)
          withState(ss)(exec)
          emitString("}")
          emitString("}")
        } else {
          // multiple targets!
          for ((target, returns) <- retframes) {
            assert(returns.length == 1) // not handling multiple calls to multiple targets...
            val (k,s) = returns(0)
            val ret = reify {
              if (debugMethods) emitString("// >> " + method)
              withState(s)(exec)
            }
            genGotoDef(k, ret)
          }
        }
      }
      liftConst(())
    }



    // alternative version that always execs up to the dominance frontier

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


    var xxx = 100

    def execMethodPostDom(mframe: InterpreterFrame): Rep[Unit] = {
      import scala.collection.JavaConversions._

      // obtain block mapping that will tell us about dominance relations
      val method = mframe.getMethod()
      val graalBlocks = getGraalBlocks(method, 0)
      def getGraalBlock(fr: InterpreterFrame) = graalBlocks.blocks.find(_.startBci == fr.getBCI).get

      emitString("/*")
      val postDom = postDominators(graalBlocks.blocks.toList)
      for (b <- graalBlocks.blocks) {
          emitString(b + " succ [" + postDom(b).map("B"+_.blockID).mkString(",") + "]")
      }
      emitString("*/")

      val saveHandler = handler
      val saveDepth = getContext(mframe).length

      //if (debugMethods) 
      //emitString("// << " + method)

      val (mkey,mkeyid) = contextKeyId(mframe)

      case class BlockInfo(inEdges: List[(Int,IState)], inState: IState)
      case class BlockInfoOut(returns: List[IState], gotos: List[IState], code: Block[Unit])

      val blockInfo: mutable.Map[Int, BlockInfo] = new mutable.HashMap
      val blockInfoOut: mutable.Map[Int, BlockInfoOut] = new mutable.HashMap

      var worklist: List[Int] = Nil

      var curBlock = -1

      // post dominators of current block
      var frontier: Set[BciBlockMapping.Block] = Set.empty 
      var frontierX: BciBlockMapping.Block = null
      var frontierL: BciBlockMapping.Block = null
      var frontierY: InterpreterFrame = null

      // *** entry point: main control transfer handler ***
      handler = { blockFrame =>
        val d = getContext(blockFrame).length

        xxx -= 1
        //println(xxx)
        if (xxx == 0) assert(false,"budget!")

        if (d > saveDepth) execMethodPostDom(blockFrame)
        else if (d < saveDepth) { 
          handler = saveHandler

          //emitString("// >> " + method)

          exec(blockFrame) // pass on to next handler one level up
        } else {
          gotoBlock(blockFrame)
        }
      }

      def gotoBlock(blockFrame: InterpreterFrame): Rep[Unit] = {
        // make sure we're still in the same method! --> catch external calls that don't reset handler
        //assert(mframe.getMethod == blockFrame.getMethod, {"\n" +
        //        mframe.getMethod + "\n" +
        //        blockFrame.getMethod})

        // do not exec past control flow joins (only up to post-dom frontier)

        val b = getGraalBlock(blockFrame)
        if (b.isLoopHeader) {
          println("XXX loop header " + b + "/" + postDom(b))
        } else {
          println("YYY normal      " + b + "/" + postDom(b))
        }
        if (frontierX == b) { // hit next block
          //emitString("// bail out: " + b + " in frontier " + frontier)
          if (frontierY != null)
            emitString("// XXX ignore previous lub data " + frontierY)
          frontierY = blockFrame // TODO: lub and backpatch
        } else if (frontierL == b) {  // hit loop back-edge
          //emitString("// bail out: " + b + " in frontier " + frontier)
          //if (frontierY != null)
            emitString("loop() // XXX continue")
          //frontierY = blockFrame // TODO: lub and backpatch
        } else {
          val safeFrontier = frontier
          val safeFrontierX = frontierX
          val safeFrontierL = frontierL
          try {
            //emitString("{// >> gotoBlock "+contextKey(blockFrame))
            frontier = postDom(b) - b // TODO: loops?
            if (frontier.nonEmpty)
              frontierX = frontier.toList.sortBy(_.blockID).head  // TODO: right order?
            else
              frontierX = null
            println("frontierX = "+frontierX)
            if (b.isLoopHeader) {
              emitString("loop(); def loop() {")
              frontierL = b
            } else
              frontierL = null
            println("frontierL = "+frontierL)
            emitString("// " + b + " --> " + frontier)
            execFoReal(blockFrame);
          } finally {
            if (b.isLoopHeader) {
              emitString("}")
            }
            //emitString("}// << gotoBlock "+contextKey(blockFrame))
            //emitString("// continue: " + frontierX + "/" + frontierY) 
            frontier = safeFrontier
            frontierX = safeFrontierX
            frontierL = safeFrontierL
            if (frontierY != null) {
              val next = frontierY
              frontierY = null // lub?
              gotoBlock(next)
            }
          }
        }
      }

      gotoBlock(mframe)

      /* why doesn't this work?? 

      while (frontierY != null) {
        val next = frontierY
        frontierY = null // lub?
        gotoBlock(next)
      }
      */

      liftConst(())
    }

    // need to override if we're using the post-dom version: since we're not using
    // CPS, we need to compute joins after the conditional
    override def if_[T:TypeRep](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T] = {
      super.if_(x) {
        y
        // get state A, placeholder JA
      } {
        z
        // get state B, placeholder JB
      }
      // set state lub(A,B), backpatch JA/JB
    }





    // halt on basic block boundaries: do not silently skip into the next block
    // (identify block start bci by looking at BasicBlockMapping)

    override def executeInstruction(frame: InterpreterFrame, bs: BytecodeStream): Control = {
      import collection.JavaConversions._
      val c = super.executeInstruction(frame,bs)

      val graalBlock = getGraalBlocks(frame.getMethod,0)

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

    override def reset {
      graalBlockMapping.clear
      super.reset
    }
}