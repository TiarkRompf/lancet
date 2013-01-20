package lancet.interpreter

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;

import com.oracle.graal.java.BciBlockMapping

import scala.collection.{mutable,immutable}

// (done) track reads and writes through constants --> elim reads
// (todo) cse --> elim redundant checks
// (todo) flow sensitive conditionals --> elim redundant branches


class BytecodeInterpreter_Opt extends BytecodeInterpreter_Opt4


// version 4 -- reverse engineer more of the program block structure (if, loop)


class BytecodeInterpreter_Opt4 extends BytecodeInterpreter_Str with RuntimeUniverse_Opt {
    override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m)
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
              println("val "+str+" = " + b + " // LUBC(" + a + "," + b + ")")
            val tp = (if (b == null) a.typ else b.typ).asInstanceOf[TypeRep[AnyRef]] // NPE? should take a.typ in general?
            val phi = Dyn[AnyRef](str)(tp)
            y.locals(i) = phi
          }
        }

        lub(x.getParentFrame, y.getParentFrame)
        y
      }
    }


    // config options

    var debugBlocks = false
    var debugMethods = false
    var debugReturns = false
    var debugLoops = false
    var debugPaths = false

    //var emitControlFlow = true
    var emitRecursive = false

    var budget = 10000

    // internal data structures

    var worklist: IndexedSeq[InterpreterFrame] = Vector.empty

    val graalBlockMapping = new mutable.HashMap[ResolvedJavaMethod, BciBlockMapping] // map key to store


    val stats = new mutable.HashMap[String, Int] // map key to id

    val info = new mutable.HashMap[String, Int] // map key to id
    var count = 0

    val storeInfo = new mutable.HashMap[String, List[StoreLattice.Elem]] // map key to store
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

    def freshFrameSimple(frame: InterpreterFrame): InterpreterFrame_Str = if (frame eq null) null else {
      val frame2 = frame.asInstanceOf[InterpreterFrame_Str].copy2(freshFrameSimple(frame.getParentFrame))
      val depth = frame2.depth
      frame2
    }

    def getAllArgs(frame: InterpreterFrame) = frame.getReturnValue()::getContext(frame).dropRight(1).flatMap(_.asInstanceOf[InterpreterFrame_Str].locals)


    // exec loop

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


    var handler: (InterpreterFrame => Rep[Unit]) = execMethod
    var depth = 0

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



    // calc lubs and backpatch info for jumps
    type State = (InterpreterFrame, StoreLattice.Elem)    
    def allLubs(states: List[State]): (State,List[String]) = {
      val gos = states map { case (frameX,storeX) =>
        val frameY = freshFrameSimple(frameX)
        var storeY = storeX
        val (go, _) = captureOutputResult {
          states foreach { case (f,s) => 
            FrameLattice.lub(f, frameY) 
            storeY = StoreLattice.lub(s, storeY)
          }
          //val locals = FrameLattice.getFields(frameY).filter(_.toString.startsWith("PHI"))
          //val fields = StoreLattice.getFields(storeY).filter(_.toString.startsWith("LUB"))
          //for (v <- locals ++ fields) println("v"+v+" = "+v)
        }
        (go,frameY,storeY)
      }
      /* assert all match ...
      assert(contextKey(f02) == contextKey(f12))
      assert(getAllArgs(f02) == getAllArgs(f12))
      assert(s02 == s12)
      */
      val (_,f02,s02) = gos(0)
      ((f02,s02),gos.map(_._1))
    }

    def getFields(s: State) = {
      val locals = FrameLattice.getFields(s._1).filterNot(_.isInstanceOf[Static[_]])//filter(_.toString.startsWith("PHI"))
      val fields = StoreLattice.getFields(s._2).filterNot(_.isInstanceOf[Static[_]])//.filter(_.toString.startsWith("LUB"))
      (locals ++ fields).distinct.sortBy(_.toString)
    }


    def execMethod(mframe: InterpreterFrame): Rep[Unit] = {
      import scala.collection.JavaConversions._

      // obtain block mapping that will tell us about dominance relations
      val method = mframe.getMethod()

      val graalBlock = graalBlockMapping.getOrElseUpdate(method, {
        val map = new BciBlockMapping(method);
        map.build();
        /*println("/*")
        println("block map for method " + method)
        for (b <- map.blocks) {
          println(b + " succ [" + b.successors.map("B"+_.blockID).mkString(",") + "]")
        }
        println("*/")*/ 
        map
      })

      def postDominators(blocks: List[BciBlockMapping.Block]) = {
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

      var returns: List[State] = Nil
      var gotos: List[State] = Nil

      val blockInfo: mutable.Map[Int, (List[Int],State)] = new mutable.HashMap

      var worklist: List[Int] = Nil

      val replaceGoto: mutable.Map[Int,String] = new mutable.HashMap
      val replaceBlock: mutable.Map[Int,String] = new mutable.HashMap

      def getGraalBlock(fr: InterpreterFrame) = graalBlock.blocks.find(_.startBci == fr.getBCI).get

      handler = { blockFrame =>

        val d = getContext(blockFrame).length

        if (d > saveDepth) execMethod(blockFrame)
        else if (d < saveDepth) { 
          returns = returns :+ (freshFrameSimple(blockFrame), store)
          println("RETURN_"+(returns.length-1))
          liftConst(())
        } else {
          gotoBlock(blockFrame)
        }
      }

      def statesDiffer(s0: State, s1: State) = 
        getAllArgs(s0._1) != getAllArgs(s1._1) || s0._2 != s1._2

      def gotoBlock(blockFrame: InterpreterFrame): Rep[Unit] = {
        // make sure we're still in the same method! --> catch external calls that don't reset handler
        assert(mframe.getMethod == blockFrame.getMethod, {"\n" +
                mframe.getMethod + "\n" +
                blockFrame.getMethod})

        val s = (freshFrameSimple(blockFrame), store)
        val b = getGraalBlock(blockFrame)
        blockInfo.get(b.blockID) match {
          case Some((gs, state)) => 
            val (_,(state2,_)) = captureOutputResult(allLubs(List(state,s))) // suppress output
            blockInfo(b.blockID) = (gs:+gotos.length, state2)
            if (!worklist.contains(b.blockID) && statesDiffer(state,state2)) {
              worklist = (b.blockID::worklist).sorted
            }
            // if (state != state2) worklist += b.blockID
          case None => 
            blockInfo(b.blockID) = (gotos.length::Nil,s)
            worklist = (b.blockID::worklist).sorted
        }
        //blockInfo(b.blockID) = blockInfo.getOrElse(b.blockID,Nil) :+ (gotos.length,s)

        gotos = gotos :+ s
        println("GOTO_"+(gotos.length-1)+";")
        liftConst(())

        /*val key = contextKey(frame)
        var fresh = false
        val id = info.getOrElseUpdate(key, { val id = count; count += 1; fresh = true; id })
        var cnt = storeInfo.getOrElse(key, Nil).length
        println("//GOTO_"+id+" // "+key)*/
      }

      val (src0, res) = captureOutputResult {
        gotoBlock(mframe) // returns on first goto

        while (worklist.nonEmpty) {
          val i = worklist.head
          worklist = worklist.tail
          val b = graalBlock.blocks(i)

          assert (blockInfo.contains(i))
          val (gs,ss) = blockInfo(i)
          val (f02,s02) = ss
          val (src,_) = captureOutputResult {
            store = s02
            execPlain(f02)
          }
          replaceBlock(i) = src
        }

        for (b <- graalBlock.blocks) {
          val i = b.blockID

          if (blockInfo.contains(i)) {
          val (gs,ss) = blockInfo(i)

          if (gs.length == 1) {
            val (go, (f02,s02)) = (gs.head, ss)
            replaceGoto(go) = replaceBlock(i)
          } else {
            val (f12,s12) = ss
            val fields = getFields(ss)
            val key = contextKey(f12)
            val keyid = info.getOrElseUpdate(key, { val id = count; count += 1; id })

            for (g <- gs) {
              val (f02,s02) = gotos(g)
              val (_,_::head::_) = allLubs(List((f12,s12),(f02,s02)))
              replaceGoto(g) = ";{"+head.trim + "\nBLOCK_"+keyid+"("+fields.mkString(",")+")}"
            }

            println("// "+key)
            println("def BLOCK_"+keyid+"("+fields.map(v=>v+":"+v.typ).mkString(",")+"): Unit = {")
            println(indented(replaceBlock(i)))
            println("}")
          }}

        }


        liftConst(())
      }


      // XXXXXXX note: GOTO_i and RETURN_i may be in blocks that have been overwritten
      // (contents of gotos and returns are never discarded)
      // so if there are loops, we may assume there are more gotos/returns than actually
      // Q: does this occur anywhere?

      var src = src0
      for (i <- 0 until gotos.length if replaceGoto.contains(i)) {
        src = src.replace("GOTO_"+i+";", replaceGoto(i).trim)
        //src = src + "def GOTO_"+i+": Unit = {\n" + indented(replaceGoto(i).trim) + "\n}\n"
      }

      handler = saveHandler

      if (debugMethods) println("// >> " + method + " " + returns.length)

      if (returns.length == 0) {
        print(src)
        println("// (no return?)")
      } else if (returns.length == 1) { 

        val (frame0,store0) = returns(0)
        /*val locals = FrameLattice.getFields(frame0).filter(_.toString.startsWith("PHI"))
        val fields = StoreLattice.getFields(store0).filter(_.toString.startsWith("LUB"))
        for (v <- locals ++ fields) 
          println("var v"+v+" = null.asInstanceOf["+v.typ+"]")
        print(src.replace("RETURN_0", "")) // "continue"))
        for (v <- locals ++ fields) 
          println("val "+v+" = v"+v)*/          
        val (retSrc,res) = captureOutputResult {
          if (debugReturns) println("// ret single "+method)
          store = store0
          exec(frame0)
        }
        print(src.replace("RETURN_0", retSrc.trim)) // "continue"))
        res

      } else {
        println("// WARNING: multiple returns ("+returns.length+") in " + mframe.getMethod)
        /*println("/*")
        for ((bc,i) <- mframe.getMethod.code.zipWithIndex)
          println(i+": "+Bytecodes.baseNameOf(bc))
        println("*/")*/


        val (ss@(f02,s02), gos) = allLubs(returns)

        val fields = getFields(ss)

        println(";{")
        for (v <- fields) 
          println("var v"+v+" = null.asInstanceOf["+v.typ+"]")

        val assign = fields.map { v =>
          "v"+v+" = "+v
        }.mkString("\n")

        var src1 = src
        for ((go,i) <- gos.zipWithIndex) {
          src1 = src1.replace("RETURN_"+i,"/*R"+i+"*/" + go+assign)
        }

        print(src1)
        
        println(";{")
        if (debugReturns) println("// ret multi "+method)
        for (v <- fields) 
          println("val "+v+" = v"+v)
        store = s02
        exec(f02)
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



    var path: List[(Int,InterpreterFrame,StoreLattice.Elem)] = Nil

    def execPlain(frame: InterpreterFrame): Rep[Unit] = {
      // decision to make: explore block afresh or generate call to existing one

      var fresh = false
      val key = contextKey(frame)
      val id = info.getOrElseUpdate(key, { val id = count; count += 1; fresh = true; id })
      var cnt = storeInfo.getOrElse(key, Nil).length

      stats(key) = stats.getOrElse(key,0) + 1

      // debug test1c
      if (key.startsWith("99:<BufferedWriter.write>")) {
        System.out.println("kick it ...")
        for ((k,v) <- store.toList.sortBy(_._1))
          System.out.println("  " + k + "  --->  "+v)
      }



/*
      if (key.contains("AssertionError.<init>"))
        return reflect[Unit]("throw new AssertionError")
*/

      //if (key.contains("StreamEncoder")) //CharsetEncoder.encode")) // encodeLoop"))
      
      if (key.contains("CharsetEncoder.encode"))
        return reflect[Unit]("WARN // refuse " + key)

      if (key.contains("Exception.<init"))
        return reflect[Unit]("WARN // refuse " + key)

      if (debugBlocks) println("// *** " + key)
      //println("// *** " + store)


      // -------------------------------------------- code below is from version 2
      var save = path
      var saveStore = store

      if (debugPaths) println("// " + path.map(_._1).mkString(" "))

      if (path.exists(_._1 == -id)) { // we're already recording a loop, tie back recursive call

        val block = captureOutput { // use a block to redefine LUB and PHI local vals
        val (frOld,stOld) = path.collectFirst { case (`id`, frOld, stOld) => (frOld,stOld) }.get

        FrameLattice.lub(frOld, frame)

        store = StoreLattice.lub(stOld, store) // create LUB_X definitions

        //assert(store == stOld) this may happen -- see test3.scala, TODO: but we should still check we've converged

        val locals = FrameLattice.getFields(frame).filter(_.toString.startsWith("PHI"))
        val localsStr = locals.toList.map(_.toString).sorted.mkString(",")

        val fields = StoreLattice.getFields(store).filter(_.toString.startsWith("LUB"))
        val fieldsStr = fields.toList.map(_.toString).sorted.mkString(",")

        "loop"+id+"("+localsStr+")"+"("+fieldsStr+")"
        }
        return reflect[Unit]("{" + block + "}") // may be a bad idea, but saves a line of output
      }


      val frame3 = freshFrameSimple(frame)
      var frame4 = frame3

      if (path.takeWhile(_._1 >= 0).exists(_._1 == id)) { // we're noting that we're in a loop, generalize
        
        path = (-id,freshFrameSimple(frame),store)::path

        if (debugLoops) println("// LOOP " + id + "/" + key)

        // generalize, set store to lub(store before 1st iter, store before 2nd iter = here)
        // do until fixpoint (or fail if not reached immediately)

        val (frOld,stOld) = path.collectFirst { case (`id`, frOld, stOld) => (frOld,stOld) }.get

        if (debugLoops) println("// frameBefore " + getAllArgs(frOld))
        if (debugLoops) println("// frameAfter  " + getAllArgs(frame))

        FrameLattice.lub(frOld, frame3)

        if (debugLoops) println("// stBefore " + stOld)

        if (debugLoops) println("// stAfter  " + store)

        store = StoreLattice.lub(stOld, store)

        if (debugLoops) println("// stNew    " + store)

        val locals = FrameLattice.getFields(frame3).filter(_.toString.startsWith("PHI"))
        val localsStr = locals.toList.map(s=>s+":"+s.typ).sorted.mkString(",")

        val fields = StoreLattice.getFields(store).filter(_.toString.startsWith("LUB"))
        val fieldsStr = fields.toList.map(s=>s+":"+s.typ).sorted.mkString(",")

        println("def loop"+id+"("+localsStr+")"+"("+fieldsStr+"): Unit = {")

        frame4 = freshFrameSimple(frame3) // copy again

      } else {
        path = (id,freshFrameSimple(frame),store)::path
      }

      val bci = frame3.getBCI()
      val bs = new BytecodeStream(frame3.getMethod.code())
      //bs.setBCI(globalFrame.getBCI())
      val res1 = try { executeBlock(frame3, bs, bci) } catch {
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

      path = save 

      val res2 = if (path.takeWhile(_._1 >= 0).exists(_._1 == id)) {
        println(res1)
        println("}")
        val locals = FrameLattice.getFields(frame4).filter(_.toString.startsWith("PHI"))
        val localsStr = locals.toList.map(_.toString).sorted.mkString(",")
        val fields = StoreLattice.getFields(store).filter(_.toString.startsWith("LUB"))
        val fieldsStr = fields.toList.map(_.toString).sorted.mkString(",")
        reflect[Unit]("loop"+id+"("+localsStr+")"+"("+fieldsStr+")")
      } else {
        res1
      }

      store = saveStore // reset here or earlier?

      return res2



      // -------------------------------------------- code below is from version 1


      // create a block ....

      // copy whole stack. TODO: not ideal

      val frame2 = freshFrame(frame)

      val params = getAllArgs(frame2)
      val args = getAllArgs(frame)

      // check if store lattice has changed ...
      val store2 = StoreLattice.alpha(store, args, params)

      val stOld = storeInfo.get(key) match { case Some(h::_) => h case _ => StoreLattice.bottom }
      val stNew = if (stOld.isEmpty) store2 else StoreLattice.lub(stOld,store2) // create phi defs as side effect


      println("// args:   " + args)
      println("// store:  " + store)
      println("// params: " + params)
      println("// store2: " + store2)
      println("// stNew:  " + stNew)

      def domain(st: StoreLattice.Elem) = st collect { case (k,Partial(fs)) => k } toSet;

      val extraNow = domain(store)
      val extraNew = domain(stNew)

      val extra = extraNow intersect extraNew
      val extraNull = extraNew diff extra


      //assert((domain(stOld) intersect domain(stNew)) == domain(stOld)) // can only grow domain (?)

      // decision split vs generalize: use store2 -> split, stNew -> generalize

      if (stNew != stOld || fresh) { // need to update: enqueue worklist item
        if (stNew != stOld)
          println("// != old  " + stOld)

        storeInfo(key) = stNew::storeInfo.getOrElse(key, Nil)

        if (worklist.exists(f => contextKey(f) == key)) {
          println("// overtaking " + id + "_" + cnt + "/" + key)
          if (domain(stOld) != domain(stNew))
            println("// domain " + domain(stOld) + " --> " + domain(stNew))
        }

        worklist = worklist :+ frame2 // TODO: don't enqueue twice
        // note: must not rely on stNew when generating call! (later, for now its ok)

        cnt += 1
      }

      val lubs = StoreLattice.getDynFields(stNew).filterNot { d => 
        extra.contains(d.toString) || d.toString.startsWith("p") }
      println("// lub params " + lubs)

      // role of 'cnt': we do not overwrite speculative results but emit all
      // generated variants. earlier calls still go to the preliminary versions.

      reflect[Unit]("block_"+id+"_"+(cnt-1)+"("+args.mkString(","),")" +
        "(" + (extra.map(s=>s+"="+s) ++ extraNull.map(s=>s+"=null")).mkString(",") + ")" +
        "(" + (lubs.map(s=>s+"="+s).mkString(",")) + ")" +
        " // "+key)
    }


    // TODO: can't translate blocks just like that to Scala methods: 
    // next block may refer to stuff defined in earlier block (need nesting, 
    // but problem with back edges)

    // may need different treatment for intra-procedural blocks and function
    // calls: want to inline functions but not generally duplicate local blocks

    def loop(root: InterpreterFrame, main: InterpreterFrame): Unit = {// throws Throwable {

      pushAsObjectInternal(root, main.getMethod.signature().returnKind(), Dyn[Object]("null /* stub return value "+main.getMethod.signature().returnKind()+" */")); // TODO: cleanup?

      while (worklist.nonEmpty) {
        var frame = worklist.head
        worklist = worklist.tail

        val key = contextKey(frame)
        val id = info(key)
        val cnt = srcInfo(id).length

        store = storeInfo(key).reverse(cnt)

        val (src, _) = captureOutputResult {

          println("// *** begin block " + key)
          val params = getAllArgs(frame)
          val paramsStr = params.map(x => if (x eq null) "?" else x.toString+":"+x.typ)
          val extra = store collect { case (k,Partial(fs)) => k } toList;
          val extraStr = extra.map(x => x+":AnyRef /*= null*/")
          val lubs = StoreLattice.getDynFields(store).filterNot {
            d => extra.contains(d.toString) || d.toString.startsWith("p") }
          val lubsStr = lubs.map(x=> x.toString+":"+x.typ)

          println("def block_"+id+"_"+cnt+"("+paramsStr.mkString(",")+")("+extraStr.mkString(",")+")("+lubsStr.mkString(",")+"): Any = {")

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



    override def compile[A:Manifest,B:Manifest](f: A=>B): A=>B = {
      val f1 = try super.compile(f) finally {
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