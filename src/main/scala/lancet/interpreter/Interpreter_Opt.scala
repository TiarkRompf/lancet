package lancet.interpreter

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

      pushAsObjectInternal(root, main.getMethod.signature().returnKind(), Dyn[Object]("null /* stub return value "+main.getMethod.signature().returnKind()+" */")); // TODO: cleanup?

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


// (done) track reads and writes through constants --> elim reads
// (todo) cse --> elim redundant checks
// (todo) flow sensitive conditionals --> elim redundant branches




class BytecodeInterpreter_Opt extends BytecodeInterpreter_Str with RuntimeUniverse_Opt {
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

        assert(x.locals.length == y.locals.length)

        for (i <- 0 until y.locals.length) {
          val a = x.locals(i)
          val b = y.locals(i)

          if (a != b) {
            val str = "PHI_"+x.depth+"_"+i
            if (b.toString != str)
              println("val "+str+" = " + b + " // LUBC(" + a + "," + b + ")")
            val tp = b.typ.asInstanceOf[TypeRep[AnyRef]] // NPE?
            val phi = Dyn[AnyRef](str)(tp)
            y.locals(i) = phi
          }
        }

        lub(x.getParentFrame, y.getParentFrame)
        y
      }
    }


    // config options

    var debugLoops = false
    var debugPaths = false

    var emitControlFlow = true
    var emitRecursive = false

    var budget = 10000

    // internal data structures

    var worklist: IndexedSeq[InterpreterFrame] = Vector.empty

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

    var path: List[(Int,InterpreterFrame,StoreLattice.Elem)] = Nil

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
      

      // decision to make: explore block afresh or generate call to existing one

      var fresh = false
      val key = contextKey(frame)
      val id = info.getOrElseUpdate(key, { val id = count; count += 1; fresh = true; id })
      var cnt = storeInfo.getOrElse(key, Nil).length


      if (key.contains("AssertionError.<init>"))
        return reflect[Unit]("throw new AssertionError")

      if (key.contains("StreamEncoder")) //CharsetEncoder.encode")) // encodeLoop"))
        return reflect[Unit]("WARN // refuse " + key)

      if (key.contains("Exception.<init"))
        return reflect[Unit]("WARN // refuse " + key)


      //println("// *** " + key)
      //println("// *** " + store)

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
        return Dyn[Unit](";{" + block + "}") // may be a bad idea, but saves a line of output
      }


      val frame3 = freshFrameSimple(frame)

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
          e.getStackTrace().take(20).map(println)
          println("*/")
          liftConst(())
      }

      path = save 

      val res2 = if (path.takeWhile(_._1 >= 0).exists(_._1 == id)) {
        println(res1)
        println("}")
        val locals = FrameLattice.getFields(frame3).filter(_.toString.startsWith("PHI"))
        val localsStr = locals.toList.map(_.toString).sorted.mkString(",")
        val fields = StoreLattice.getFields(store).filter(_.toString.startsWith("LUB"))
        val fieldsStr = fields.toList.map(_.toString).sorted.mkString(",")
        reflect[Unit]("loop"+id+"("+localsStr+")"+"("+fieldsStr+")")
      } else {
        res1
      }

      store = saveStore // reset here or earlier?

      return res2



      // --------------------------------------------


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



}