package lancet
package analysis

class TestAnalysis4 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-4"

/* 
  make loop ranges explicit, reason about
  an infinite number of memory addresses
  (allocation site indexed by loop iteration)

  TODO -- WORK IN PROGRESS
*/

/*
TODO: 
- switch to optimistic? (can we even talk about opt/pess here?)
- make sense of inequalities/recurrences (partially done)
*/

  object Test1 {

    // *** util

    def captureOutputResult[A](func: => A): (String,A) = {
      import java.io._
      val bstream = new ByteArrayOutputStream
      val r = withOutput(new PrintStream(bstream))(func) //func
      (bstream.toString, r)
    }
    def withOutput[A](out: java.io.PrintStream)(func: => A): A = {
      //val oldStdOut = System.out
      //val oldStdErr = System.err
      try {
        //System.setOut(out)
        //System.setErr(out)
        scala.Console.withOut(out)(scala.Console.withErr(out)(func))
      } finally {
        out.flush()
        out.close()
        //System.setOut(oldStdOut)
        //System.setErr(oldStdErr)
      }
    }


    // *** intermediate language / IR interfaces

    abstract class GVal {
      override def toString: String = this match {
        case GRef(s)   => s
        case GConst(x: String) => "\""+x+"\""
        case GConst(x) => s"$x"
      }
    }

    case class GRef(s: String) extends GVal
    case class GConst(x: Any) extends GVal

    abstract class Def {
      override def toString: String = mirrorDef(this, DString)
    }

    case class DMap(m: Map[GVal,GVal]) extends Def
    case class DUpdate(x: GVal, f: GVal, y: GVal) extends Def
    case class DSelect(x: GVal, f: GVal) extends Def
    case class DPlus(x: GVal, y: GVal) extends Def
    case class DTimes(x: GVal, y: GVal) extends Def
    case class DLess(x: GVal, y: GVal) extends Def
    case class DEqual(x: GVal, y: GVal) extends Def
    case class DPair(x: GVal, y: GVal) extends Def
    case class DIf(c: GVal, x: GVal, y: GVal) extends Def
    case class DFixIndex(x: String, c: GVal) extends Def
    case class DCall(f: GVal, x: GVal) extends Def
    case class DFun(f: String, x: String, y: GVal) extends Def
    case class DOther(s: String) extends Def

    def mirrorDef(d: Def, dst: DIntf { type From >: GVal }): dst.To = d match {
      case DMap(m)                            => dst.map(m.asInstanceOf[Map[dst.From, dst.From]])
      case DUpdate(x: GVal, f: GVal, y: GVal) => dst.update(x,f,y)
      case DSelect(x: GVal, f: GVal)          => dst.select(x,f)
      case DPlus(x: GVal, y: GVal)            => dst.plus(x,y)
      case DTimes(x: GVal, y: GVal)           => dst.times(x,y)
      case DLess(x: GVal, y: GVal)            => dst.less(x,y)
      case DEqual(x: GVal, y: GVal)           => dst.equal(x,y)
      case DPair(x: GVal, y: GVal)            => dst.pair(x,y)
      case DIf(c: GVal, x: GVal, y: GVal)     => dst.iff(c,x,y)
      case DFixIndex(x: String, c: GVal)      => dst.fixindex(x,c)
      case DCall(f: GVal, x: GVal)            => dst.call(f,x)
      case DFun(f: String, x: String, y: GVal)=> dst.fun(f,x,y)
      case DOther(s: String)                  => dst.other(s)
    }

    trait DIntf {
      type From
      type To
      def map(m: Map[From,From]): To
      def update(x: From, f: From, y: From): To
      def select(x: From, f: From): To
      def plus(x: From, y: From): To
      def times(x: From, y: From): To
      def less(x: From, y: From): To
      def equal(x: From, y: From): To
      def pair(x: From, y: From): To
      def iff(c: From, x: From, y: From): To
      def fixindex(x: String, c: From): To
      def call(f: From, x: From): To
      def fun(f: String, x: String, y: From): To
      def other(s: String): To
    }

    object DString extends DIntf {
      type From = Any
      type To = String
      def map(m: Map[From,From])            = s"$m"
      def update(x: From, f: From, y: From) = s"$x + ($f -> $y)"
      def select(x: From, f: From)          = s"$x($f)"
      def plus(x: From, y: From)            = s"$x + $y"
      def times(x: From, y: From)           = s"$x * $y"
      def less(x: From, y: From)            = s"$x < $y"
      def equal(x: From, y: From)           = s"$x == $y"
      def pair(x: From, y: From)            = s"($x,$y)"
      def iff(c: From, x: From, y: From)    = s"if ($c) $x else $y"
      def fixindex(x: String, c: From)      = s"fixindex($x => $c)"
      def call(f: From, x: From)            = s"$f($x)"
      def fun(f: String, x: String, y: From)= s"{ $x => $y }"
      def other(s: String)                  = s
    }

    object DDef extends DIntf {
      type From = GVal
      type To = Def
      def map(m: Map[From,From])            = DMap(m)
      def update(x: From, f: From, y: From) = DUpdate(x,f,y)
      def select(x: From, f: From)          = DSelect(x,f)
      def plus(x: From, y: From)            = DPlus(x,y)
      def times(x: From, y: From)           = DTimes(x,y)
      def less(x: From, y: From)            = DLess(x,y)
      def equal(x: From, y: From)           = DEqual(x,y)
      def pair(x: From, y: From)            = DPair(x,y)
      def iff(c: From, x: From, y: From)    = DIf(c,x,y)
      def fixindex(x: String, c: From)      = DFixIndex(x,c)
      def call(f: From, x: From)            = DCall(f,x)
      def fun(f: String, x: String, y: From)= DFun(f,x,y)
      def other(s: String)                  = DOther(s)
    }

    trait DXForm extends DIntf {
      type From
      type To
      val next: DIntf
      def pre(x: From): next.From
      def post(x: next.To): To
      def map(m: Map[From,From])            = post(next.map(m.map(kv=>pre(kv._1)->pre(kv._2))))
      def update(x: From, f: From, y: From) = post(next.update(pre(x),pre(f),pre(y)))
      def select(x: From, f: From)          = post(next.select(pre(x),pre(f)))
      def plus(x: From, y: From)            = post(next.plus(pre(x),pre(y)))
      def times(x: From, y: From)           = post(next.times(pre(x),pre(y)))
      def less(x: From, y: From)            = post(next.less(pre(x),pre(y)))
      def equal(x: From, y: From)           = post(next.equal(pre(x),pre(y)))
      def pair(x: From, y: From)            = post(next.pair(pre(x),pre(y)))
      def iff(c: From, x: From, y: From)    = post(next.iff(pre(c),pre(x),pre(y)))
      def fixindex(x: String, c: From)      = post(next.fixindex(x,pre(c)))
      def call(f: From, x: From)            = post(next.call(pre(f),pre(x)))
      def fun(f: String, x: String, y: From)= post(next.fun(f,x,pre(y)))
      def other(s: String)                  = post(next.other(s))
    }

    object IRS extends DXForm {
      type From = String
      type To = String
      val next = DString
      def const(x: Any) = s"$x"
      def pre(x: String) = x
      def post(x: String): String = reflect(x)
      override def fun(f: String, x: String, y: From) = reflect(f,next.fun(f,x,pre(y)))
    }
    object IRS_Term extends DXForm {
      type From = GVal
      type To = String
      val next = DString
      def const(x: Any) = s"$x"
      def pre(x: GVal) = findDefinition(x.toString).map(d=>mirrorDef(d,this)).getOrElse(x.toString)
      def preBlock(x: GVal) = {
        val s = pre(x)
        if (s startsWith "if") s"{\n  ${s.replace("\n","\n  ")}\n}"
        else s
      }
      def post(x: String): String = x
      var rec: List[String] = Nil
      def reset = rec = Nil // HACK
      override def fun(f: String, x: String, y: From) = if (rec contains f) f else {
        rec ::= f; reflect(f,next.fun(f,x,pre(y)))
      }
      override def iff(c: From, x: From, y: From) = post(next.iff(pre(c),preBlock(x),preBlock(y)))
    }

    object IRD extends DXForm {
      type From = GVal
      type To = GVal
      val next = DDef
      def const(x: Any) = GConst(x)
      def pre(x: GVal) = x
      def post(x: Def): GVal = dreflect(x)
      //override def fun(f: String, x: String, y: From) = dreflect(f,next.fun(f,x,pre(y)))

      object Def {
        def unapply(x:GVal): Option[Def] = x match {
          case GConst(_) => None
          case GRef(s)   => findDefinition(s)
        }
      }

      // dependencies / schedule
      def syms(d: Def): List[String] = {
        var sl: List[String] = Nil
        object collector extends DXForm {
          type From = GVal
          type To = String // ignore
          val next = DString
          def pre(x: GVal) = x match { case GRef(s) => sl ::= s; s case _ => "" }
          def post(x: String) = x
          //override def fun(f: String,x: String,y: GVal) = ""
        }
        mirrorDef(d,collector)
        sl
      }
      def boundSyms(d: Def): List[String] = d match { case DFun(f,x,y) => List(f,x) case _ => Nil }
      def deps(st: List[String]): List[(String,Def)] =
        globalDefs.filter(p=>st contains p._1) // TODO: opt
      def schedule(x: GVal) = {
        val start = x match { case GRef(s) => List(s) case _ => Nil }
        val xx = scala.virtualization.lms.util.GraphUtil.stronglyConnectedComponents[(String,Def)](deps(start), t => deps(syms(t._2)))
        xx.flatten.reverse
      }

      def printStm(p: (String,Def)) = println(s"val ${p._1} = ${p._2}")
      def printTerm(p: GVal) = {
        IRS_Term.reset
        println(mirrorDef(findDefinition(p.toString).get,IRS_Term))
      }

      def dependsOn(a: GVal, b: GVal) = schedule(a).exists(p => GRef(p._1) == b || syms(p._2).contains(b.toString))

      // evaluate with substitution, i.e. compute trans closure of subst
      def substTrans(env0: Map[GVal,GVal]): Map[GVal,GVal] = {
        var env = env0
        object XXO extends DXForm {
          type From = GVal
          type To = GVal
          val next = IRD
          def pre(x: GVal) = {/*println(s"pre $x / $env");*/ env.getOrElse(x,x) }
          def post(x: GVal) = x
          override def fun(f: String, x: String, y: From) = {
            //println(s"not changing fundef $f $x $y -> ${pre(y)}")
            GRef(f) // don't transform fundef
          }
        }
        for ((e,d) <- globalDefs.reverse) {
          val e2 = mirrorDef(d,XXO)
          //println(s"$e -> $e2 = $d")
          if (e2 != GRef(e))
            env = env + (GRef(e) -> e2)
        }
        // need to iterate because of sched order
        if (env == env0) env else substTrans(env)
      }

      // perform iterative optimizations
      def iterateAll(res: GVal): GVal = {
        // need multiple passes? atm no
        val res1 = iterateSplitFunctions(res)
        iterateLoopInv(res1)
      }

      def iterateSplitFunctions(res: GVal): GVal = {
        println("*** begin iterate split funcs: "+res)

        val sched = schedule(res)

        val funs = sched collect { case p@(a,DFun(f,x,z)) => p }
        val calls = sched collect { case p@(a,DCall(f,y)) => p }

        println("funs:")
        funs foreach printStm

        println("calls:")
        calls foreach printStm

        // base case: eval function bodies for i = 0

        val subst = funs map {
          case (a,DFun(f,x,z)) =>
            GRef(x) -> GConst(0) 
            // if we try 1 instead, result will not be a map, but a sequence of
            // update ops extending f(0)
        }

        println("subst: "+subst.toMap)
        val zeroSubst = substTrans(subst.toMap)

        val zeros = funs map {
          case (a,DFun(f,x,z)) =>
            a -> zeroSubst.getOrElse(z,z) // alt: this.subst(z,GRef(x),GConst(0))
        }

        println("zeros: "+zeros.toMap)

        var recFuns = Set[String]() // known to remain recursive. initially empty.
        def iter: GVal = {
          // TODO:
          // It seems like we need to do something more involved if the
          // address written to depends on the loop index. The zero iteration
          // will produce something like this, with f0 being f(i) evaluated
          // at i = 0:
          //   Map(f0 -> ...)
          // However, this does not tell us anything about field f(i) in general.
          // Specializing the general case for f0 will not help.
          //
          // For allocations in loops, the key does not even exist before
          // the loop. Thus it will not be part of the map!
          //
          // A possible solution is to really take i=0 as the value after
          // the first iterations.

          // induction case: if zero iteration evaluates to a map, split function
          def mkey(f: String, x: GVal) = x match {
            case GConst(s) => f+"_"+s
            case GRef(s) => f+"_"+s
          }

          // replace all calls. TODO: handle isolated fixindex nodes?
          val xform = calls flatMap {
            case (a,DCall(f,z)) if !recFuns.contains(f.toString) =>
              zeros.toMap.apply(f.toString) match {
                case Def(DMap(m)) =>
                  println("specializing for fields " + m.keys)

                  def func(k: GVal) = GRef(mkey(f.toString,k))
                  def arg(k: GVal) = z match {
                    case Def(DFixIndex(x,`f`)) => fixindex(x, func(k)) // OLD
                    case _ => z
                  }
                  List(GRef(a) -> map(m map (kv => kv._1 -> call(func(kv._1), arg(kv._1)))))
                case _ => Nil
              }
            case _ => Nil
          }

          println("xform: "+xform.toMap)
          val xformSubst = substTrans(xform.toMap)

          // find conflicts: f(i).fields != f(0).fields
          val confl = funs flatMap {
            case (a,DFun(f,x,z)) =>
              zeros.toMap.apply(f.toString) match {
                case Def(DMap(m)) =>
                  // we need to see if f's body is a map, too (and check the set of fields)
                  xformSubst.getOrElse(z,z) match {
                    case Def(DMap(mz)) if m.keys == mz.keys => Nil // ok
                    case Def(DMap(mz)) => println(s"XX map keys don't match: $m $mz"); List(f)
                    case Def(z1)       => println(s"XX not a map: $m $z1 "); List(f)
                    case z1            => println(s"XX not a map: $m $z1 "); List(f)
                  }
                case _ => Nil
              }
          }

          // if there are more conflicts than expected we need to start over
          if (confl.toSet != recFuns) { recFuns = confl.toSet; iter } else {

            // generate new fundefs
            funs foreach {
              case (a,DFun(f,x,z)) => 
                zeros.toMap.apply(f.toString) match {
                  case Def(DMap(m)) =>
                    def func(k: GVal) = GRef(mkey(f.toString,k))
                    def body(k: GVal) = select(xformSubst.getOrElse(z,z),k)
                    m foreach (kv => kv._1 -> fun(func(kv._1).toString,x,body(kv._1)))
                  case _ =>
                    if (xformSubst.contains(z)) {
                      // HACK -- unsafe???
                      globalDefs = globalDefs.filterNot(_._1 == f)
                      rebuildGlobalDefsCache()
                      fun(f,x,xformSubst(z))
                      println(s"### fun has been xformed: $a = $x => $z / ${xformSubst(z)}")
                    }
                }
            }

            // changed fun defs; need to mirror everything to catch
            // smart constructor rewrites
            //val xformSubst1 = substTrans(xform.toMap)
            //xformSubst1.getOrElse(res,res)
            xformSubst.getOrElse(res,res)
          }
        }
        val res1 = iter

        // FIXME: inductive const prop: calls to transformed fundef
        // inside other fundefs don't seem to be transformed..

        /*
        should we always go and update all function defs?
        val env = substTrans(Map())
        if (env.nonEmpty) {
          println("NOT CONVERGED: "+env)
          val res1 = xformSubst.getOrElse(res,res)
          val sched = schedule(res1)
          sched foreach {
            case (a,DFun(f,x,z)) => 
              // HACK -- unsafe???
              globalDefs = globalDefs.filterNot(_._1 == f)
              fun(f,x,env.getOrElse(z,z))
              println(s"### fun has been xformed: $a = $x => $z / ${env.getOrElse(z,z)}")
            case _ =>
          }
        }*/

        println("*** done iterate split funcs: "+res1)
        if (res1 != res) iterateSplitFunctions(res1) else res
      }

      def iterateLoopInv(res: GVal): GVal = {

        println("*** begin iterate loop inv: "+res)

        val sched = schedule(res)

        val funs = sched collect { case p@(a,DFun(f,x,z)) => p }
        val calls = sched collect { case p@(a,DCall(f,y)) => p }

        println("funs:")
        funs foreach printStm

        println("calls:")
        calls foreach printStm

        // base case: eval function bodies for i = 0

        val subst = funs map {
          case (a,DFun(f,x,z)) =>
            GRef(x) -> GConst(0) 
        }

        println("subst: "+subst.toMap)
        val zeroSubst = substTrans(subst.toMap)

        val zeros = funs map {
          case (a,DFun(f,x,z)) =>
            a -> zeroSubst.getOrElse(z,z) // alt: this.subst(z,GRef(x),GConst(0))
        }

        println("zeros: "+zeros.toMap)

        var recFuns = Set[String]() // known to remain recursive. initially empty.
        def iter: GVal = {
          // speculative transformation:
          // assume optimistically that all functions f are constant:
          // f(i) = f(0) for all i.
          println("exclude: "+recFuns)

          // speculatively replace calls. TODO: handle isolated fixindex nodes?
          val xform = calls flatMap {
            case (a,DCall(f,z)) if !recFuns.contains(f.toString) =>
              List(GRef(a) -> zeros.toMap.apply(f.toString))
            case _ => Nil
          }

          println("xform: "+xform.toMap)
          val xformSubst = substTrans(xform.toMap)
          println("xform done: "+xform.toMap)  // XXX doesn't seem to terminate...

          // find conflicts: f(i) != f(0)
          val confl = funs flatMap {
            case (a,DFun(f,x,z)) =>
              if (xformSubst.getOrElse(z,z) != zeros.toMap.apply(f.toString)) List(f) else Nil
          }

          // if there are more conflicts than expected we need to start over
          if (confl.toSet != recFuns) { recFuns = confl.toSet; iter } else {
            // converged, update fundefs
            funs foreach {
              case (a,DFun(f,x,z)) => 
                if (xformSubst.contains(z)) {
                  val z1 = xformSubst(z) 
                  val zval = zeros.toMap.apply(f.toString)
                  println(s"## inductive prop: $f($x) = $z --> $z1")
                  // is this safe???
                  globalDefs = globalDefs.filterNot(_._1 == f)
                  rebuildGlobalDefsCache()
                  fun(f,x,z1)
                  println(s"## $f($x)=$z $f(0)=$zval $f'($x)=$z1")
                }
            }

            // changed fun defs; need to mirror everything to catch
            // smart constructor rewrites
            val xformSubst1 = substTrans(xform.toMap)
            xformSubst1.getOrElse(res,res)
          }
        }

        val res1 = iter
        println("*** done iterate loop inv: "+res1)
        res1
      }


      def subst(x: GVal, a: GVal, b: GVal): GVal = x match {
        case `a`                 => b
        case GConst(_)           => x
        case Def(DUpdate(x,f,y)) => update(subst(x,a,b),subst(f,a,b),subst(y,a,b))
        case Def(DSelect(x,f))   => select(subst(x,a,b),subst(f,a,b))
        case Def(DMap(m))        => 

          // TODO: what if stuff is substituted to the same key??
          map(m.map(kv => subst(kv._1,a,b) -> subst(kv._2,a,b)))

        case Def(dd@DIf(c@Def(o@DEqual(u,v)),x,y)) => 
          // in general, if a implies c we can take branch x; if a refutes c, y.
          // if a & c implies that something is a constant, propagate that
          a match { 
            case Def(p@DEqual(`u`,s)) =>
              //println(s"another == flying by: $o, $p -> $b")
              if (b == const(1)) { // u == s
                if (equal(s,v) == const(1))      // u == s && s == v --> u == v:
                  return subst(x,c,const(1))
                else if (equal(s,v) == const(0)) // u == s && s != v --> u != v:
                  return subst(y,c,const(0))
              }
              if (b == const(0)) { // u != s
                if (equal(s,v) == const(1))      // u != s && s == v --> u != v:
                  return subst(y,c,const(0))
                //else if (equal(s,v) == const(0)) // u != s && s != v --> u == v:
                //  return subst(x,c,const(1))
              }
            case _ =>
          }
          iff(subst(c,a,b),subst(x,a,b),subst(y,a,b))
        case Def(dd@DIf(c@Def(o@DLess(u,v)),x,y)) => 
          // in general, if a implies c we can take branch x; if a refutes c, y.
          // if a & c implies that something is a constant, propagate that
          a match { 
            case Def(p@DLess(`u`,s)) =>
              // evaluate u < v  given that u < s or ¬(u < s)
              //println(s"another < flying by: $o, $p -> $b")
              // look for: a < x && !(a < x-1) ---> x == 1
              if (s == plus(v,const(-1))) { // other constants?
                if (b == const(1)) {
                  println(s"hit: $u<$v-1 implies $u<$v in $dd")
                  return subst(x,a,b)
                }
                if (b == const(0)) {
                  println(s"¬$u<$v-1 and $u<$v implies $u=$v-1 in $dd")
                  if (u.isInstanceOf[GConst])
                    return iff(subst(c,a,b),subst(x,s,plus(u,const(1))),subst(y,a,b))
                  else
                    return iff(subst(c,a,b),subst(x,u,s),subst(y,a,b))
                }
              }
              if (v == plus(s,const(-1))) { // other constants?
                if (b == const(0)) {
                  println(s"hit2: ¬$u<$s refutes $u<$s-1 in $dd")
                  return subst(y,a,b)
                }
                if (b == const(1)) {
                  println(s"hit2: $u<$s and ¬$u<$s-1 implies $u=$s-1 in $dd")
                  if (u.isInstanceOf[GConst])
                    return iff(subst(c,a,b),subst(x,a,b),subst(y,s,plus(u,const(1))))
                  else
                    return iff(subst(c,a,b),subst(x,a,b),subst(y,u,s))
                }
              }

            // look for: 0 < x6 && !(0 < x6 + -1) ---> x6 == 1
                // if (0 < x6) if (0 < x6 + -1) if (0 < x6 + -2) if (x6 < 102) if (x6 < 101) x100 + 1 
                // else x100 else x100 else x100 + 1 else x100 + 1 else 0

            case _ => 
          }
          iff(subst(c,a,b),subst(x,a,b),subst(y,a,b))
        case Def(DIf(c,x,y))     => iff(subst(c,a,b),subst(x,a,b),subst(y,a,b))
        case Def(DPair(x,y))     => pair(subst(x,a,b),subst(y,a,b))
        case Def(DPlus(x,y))     => plus(subst(x,a,b),subst(y,a,b))
        case Def(DTimes(x,y))    => times(subst(x,a,b),subst(y,a,b))
        case Def(o@DLess(u,v))     => 
          a match { // TODO
            case Def(p@DLess(`u`,s)) =>
              //if (v == s || less(s,v) == const(1)) return const(1)
            case _ =>
          }
          less(subst(u,a,b),subst(v,a,b))
        case Def(DEqual(x,y))    => equal(subst(x,a,b),subst(y,a,b))
        case Def(DCall(f,y))     => call(subst(f,a,b),subst(y,a,b))
        case Def(DFun(f,x1,y))   => x//subst(y,a,b); x // binding??
        case Def(DFixIndex(x,y)) => fixindex(x,subst(y,a,b))
        case Def(d)              => println("no subst: "+x+"="+d); x
        case _                   => x // TOOD
      }


      override def update(x: From, f: From, y: From): From = x match {
        case GConst("undefined") => update(dreflect(DMap(Map())),f,y) // f may be non-const
        //case GConst("undefined") => x 
        case GConst(m:Map[_,_]) if m.isEmpty => update(dreflect(DMap(Map())),f,y) // f may be non-const
        case Def(DMap(m)) => 
          f match {
            case GConst(_) => map(m + (f -> y)) // TODO: y = DIf ??
            case Def(DIf(c,u,v)) => iff(c,update(x,u,y),update(x,v,y))
            case _ => 
              // It would be nice to use f as a key even if it
              // is not a constant:
              //    map(m + (f -> y))
              // At present it is not quite clear under what conditions
              // this would work. Clearly, all keys in the map must
              // be statically known to be definitely different.
              super.update(x,f,y)
          }
        // TODO: DUpdate
        // case Def(DUpdate(x2,f2,y2)) => if (f2 == f) y2 else select(x2,f)
        case Def(DIf(c,u,v)) => iff(c,update(u,f,y),update(v,f,y))
        case _ => super.update(x,f,y)
      }
      override def select(x: From, f: From): From          = x match {
        case Def(DMap(m)) => 
          f match {
            case GConst(_) => m.getOrElse(f, GConst("undefined"))
            case Def(DIf(c,u,v)) => iff(c,select(x,u),select(x,v))
            case _ => 
              var res: GVal = const("undefined")
              for ((k,v) <- m) {
                res = iff(equal(f,k), v, res)
              }
              res
              //return super.select(x,f)
              //m.getOrElse(f, GConst("undefined"))
          }
        case Def(DUpdate(x2,f2,y2)) => iff(equal(f2,f), y2, select(x2,f))
        case Def(DIf(c,x,y)) => iff(c,select(x,f),select(y,f))
        case _ => super.select(x,f)
      }
      override def plus(x: From, y: From)            = (x,y) match {
        case (GConst(x:Int),GConst(y:Int)) => GConst(x+y)
        case (GConst(0),_) => y
        case (_,GConst(0)) => x
        case (Def(DIf(c,x,z)),_) => iff(c,plus(x,y),plus(z,y))
        // random simplifications ...
        case (GConst(c),b:GRef) => plus(b,const(c)) // CAVE: non-int consts!
        case (Def(DPlus(a,b)),_) => plus(a,plus(b,y))
        //case (Def(DTimes(a,GConst(-1))),GConst(c:Int)) => plus(a,GConst(-c)) //(-a+c)=-(-c+a)
        case _ => super.plus(x,y)
      }
      override def times(x: From, y: From)            = (x,y) match {
        case (GConst(x:Int),GConst(y:Int)) => GConst(x*y)
        case (GConst(x:Int),GConst(y:Double)) => GConst((x*y).toInt) // hacky...
        case (GConst(0),_) => GConst(0)
        case (_,GConst(0)) => GConst(0)
        case (GConst(1),_) => y
        case (_,GConst(1)) => x
        case (Def(DIf(c,x,z)),_) => iff(c,times(x,y),times(z,y))
        // random simplifications ...
        case (GConst(c),b:GRef) => times(b,const(c)) // CAVE: non-int consts!
        case (Def(DTimes(a,b)),_) => times(a,times(b,y))
        case (Def(DPlus(a,b)),c) => plus(times(a,c), times(b,c))
        case (a,Def(DPlus(b,c))) => plus(times(a,b), times(a,c))
        case _ => super.times(x,y)
      }
      override def less(x: From, y: From)            = (x,y) match {
        case (GConst(x:Int),GConst(y:Int)) => GConst(if (x < y) 1 else 0)
        case (Def(DIf(c,x,z)),_) => iff(c,less(x,y),less(z,y))
        case (_,Def(DIf(c,y,z))) => iff(c,less(x,y),less(x,z))
        // random simplifications ...
        case (GConst(0),Def(DPlus(a,GConst(b:Int)))) if b < 0 =>  less(const(-b),a)
        // 0 < -a + b  -->  a < b
        case (GConst(0),Def(DPlus(Def(DTimes(a,GConst(-1))),GConst(b:Int)))) =>  less(a,const(b))
        case (Def(DPlus(a,GConst(b:Int))),c) =>  less(a,plus(c,const(-b)))
        case _ if x == y => const(0)
        // case (GConst(0),Def(DPlus())) => y
        case _ => super.less(x,y)
      }
      override def equal(x: From, y: From)           = (x,y) match {
        case (GConst(x),GConst(y)) => GConst(if (x == y) 1 else 0)
        case (GConst(x:Int),Def(DPair(_,_))) => const(0)
        case (GConst(x:String),Def(DPair(_,_))) => const(0)
        case (Def(DPair(_,_)),GConst(x:Int)) => const(0)
        case (Def(DPair(_,_)),GConst(x:String)) => const(0)
        case (Def(DIf(c,x,z)),_) => iff(c,equal(x,y),equal(z,y))
        case (_,Def(DIf(c,y,z))) => iff(c,equal(x,y),equal(x,z))
        case _ if x == y => const(1)
        case _ => super.equal(x,y)
      }
      override def pair(x: From, y: From)            = (x,y) match {
        case (GConst(x),GConst(y)) => const((x,y))
        case _ => super.pair(x,y)
      }
      override def iff(c: From, x: From, y: From):GVal    = c match {
        case GConst(0) => y
        case GConst(_) => x
        case Def(DIf(c1,x1,y1)) => iff(c1,iff(x1,x,y),iff(y1,x,y))
        case _ if x == y => x
        // TODO: if (1 < x6) x6 < 100 else true = x6 < 100
        // Taking the else branch: x6 <= 1 implies x6 < 100, so both branches 
        // would return true, ergo condition is redundant.
        // This is a bit of a hack:
        case Def(DLess(GConst(a:Int),xx)) if { x match { 
          case Def(DLess(`xx`, GConst(b:Int))) => a<b && y == const(1) case _ => false }} => x
        // Another, similar case: if (1<x6) u-x6 else u-1 = 
        // Here we extend to if (0<x6) u-x6 else u-1 in the hope that the condition
        // becomes redundant later
        case Def(DLess(GConst(1),xx)) if subst(x,xx,const(1)) == y => iff(less(const(0),xx),x,y)
        case _ => 
          (x,y) match {
            case (Def(DMap(m1)), Def(DMap(m2))) => 
              // push inside maps
              map((m1.keys++m2.keys) map { k => k -> iff(c,m1.getOrElse(k,const("nil")),m2.getOrElse(k,const("nil")))} toMap)
            case _ =>
              // generate node, but remove nested tests on same condition
              val thenp = subst(x,c,GConst(1))
              val elsep = subst(y,c,GConst(0))

              // maybe we don't need conditional: 
              /*val thenpTry = subst(x,c,GConst(0))
              val elsepTry = subst(y,c,GConst(1))
              
              if (thenp == elsepTry && elsep == thenpTry) {
                println(s"### strange if $c $x $y")
                return x
              }*/

              super.iff(c,thenp,elsep)
          }
      }

      // LowBound(lowVal,lowBound,highBound,highVal)
      object LowBound {
        def unapply(x: GVal): Option[(GVal,GVal,GVal,GVal)] = {
          x match {
            case Def(DIf(Def(DLess(lb,hb)), lv, hv)) => Some(lv,lb,hb,hv)
            case _ => None
          }
        }
      }

      override def fixindex(x: String, c: From)       = c match {
        // Q: why exactly are we subtracting 1 ?
        // it doesn't seem quite right but appears necessary
        case Def(DLess(GRef(`x`),u)) => plus(u,const(-1))
        case _ =>
          super.fixindex(x,subst(c,less(const(0),GRef(x)),const(1)))
      }

      override def call(f: From, x: From)            = f match {
        // inline calls to non-recursive functions
        case Def(DFun(f1,x1,y1)) if !dependsOn(y1,f) =>
          //println(s"*** will inline call $f($x1) = $y1 / $x1 -> $x")
          val res = subst(y1,GRef(x1),x)
          //println(s"**** inlined $f($x1)=$y1 --> $f($x)=$res")
          res
        case _ =>
          super.call(f,x)
      }

      override def fun(f: String, x: String, y: From) = y match {
        // (0)
        // f(x) = if (0 < x) f(x-1) + d else z    --->    f(x) = x * d + z
        // f(x) = if (0 < x) f(x-1)     else z    --->    f(x) = z
        // 
        // (1)
        // f(x) = if (0 < x) 
        //            if (f(x-1) < u) f(x-1) + d else f(x-1)
        //        else z                          --->    ?
        // (tricky because of recursion in condition: first
        // transform to non-recursive condition using monotonicity?)
        // 
        // TODO:
        // (2)
        // f(x) = if (0 < x) 
        //            f(x-1) + x * c + d
        //        else z                          --->    ?
        // summing the loop variable
        // (extension: e.g.  f(x-1) + k * x  )
        case Def(DIf(zc @ Def(DLess(GConst(0),GRef(`x`))),
            incRes, zeroRes)) =>

          // alt: calc y - subst(y,x,x-1) and see if it depends on x ...
          val prevx = plus(GRef(x),const(-1))
          val prevRes = call(GRef(f),prevx)

          incRes match {
            case `prevRes` =>
              fun(f,x,zeroRes)

            case Def(DPlus(`prevRes`, d)) if true && !dependsOn(d,GRef(x)) => 
              println(s"invariant stride $d")
              println(s"result = $zeroRes + $x * $d")
              val y0 = plus(times(GRef(x),d), zeroRes)
              // Q: do we ever access below-zero values? need > 0 condition?
              val y1 = iff(zc, y0, zeroRes) // CAREFUL!!
              fun(f,x,y1)
            
            /*case d @ GConst(_) if !dependsOn(d,GRef(x)) =>  // error in iterateAll if not a const ??
              // Q: do we need this case ?? it doesn't seem to do anything
              println(s"invariant res $d")
              println(s"result = $d")
              val y1 = iff(zc,d,zeroRes)
              fun(f,x,y1)*/

            case Def(DIf(Def(DLess(`prevRes`, uprBound)), // (1)
              Def(DPlus(`prevRes`, GConst(1))),  // TODO: non-unit stride
              `prevRes`)) =>
              println(s"upper bounded result")
              println(s"result = $uprBound")
              val y0 = plus(times(GRef(x),const(1)), zeroRes)
              val y1 = iff(less(GRef(x),uprBound), y0, uprBound)
              // Q: do we ever access below-zero values? need > 0 condition?
              val y2 = iff(zc, y1, zeroRes)
              fun(f,x,y2)

            case Def(DPlus(`prevRes`,  // (2)
              Def(DPlus(Def(DTimes(GRef(`x`), GConst(-1))), GConst(d))))) if true => 
              println(s"summing the loop var: -$x+$d")
              println(s"result = - $x * ($x + 1)/2 + $x*$d")
              // (0 to n).sum = n*(n+1)/2
              val xx = GRef(x)
              val y0 = times(times(xx,plus(xx,const(1))),const(-0.5))
              val y1 = plus(y0, times(xx,const(d)))
              val y2 = iff(zc, y1, zeroRes)
              fun(f,x,y2)
              // test case result: 405450
            case _ =>
              dreflect(f,next.fun(f,x,pre(y)))            
          }

        case _ =>
          dreflect(f,next.fun(f,x,pre(y))) // reuse fun sym (don't call super)
      }

    }


    // reflect/reify

    val varCount0 = 0
    var varCount = varCount0

    val globalDefs0 = Nil 
    var globalDefs: List[(String,Def)] = globalDefs0

    var globalDefsRhs: Map[String,Def] = Map()
    var globalDefsLhs: Map[Def,String] = Map()

    def rebuildGlobalDefsCache() = { globalDefsRhs = globalDefs.reverse.toMap; globalDefsLhs = globalDefs.reverse.map(kv => (kv._2,kv._1)).toMap }

    def freshVar = { varCount += 1; "x"+(varCount - 1) }

    def reflect(x: String, s: String): String = { println(s"val $x = $s"); x }

    def reflect(s: String): String = { val x = freshVar; println(s"val $x = $s"); x }
    def reify(x: => String): String = captureOutputResult(x)._1

    def findDefinition(s: String): Option[Def] = globalDefsRhs.get(s)
      //globalDefs.reverse.collectFirst { case (`s`,d) => d }

    def dreflect(x0: => String, s: Def): GVal = globalDefsLhs.get(s).map(GRef).getOrElse {
      val x = x0; 
      globalDefs = (x->s)::globalDefs
      globalDefsRhs = globalDefsRhs + (x->s)
      globalDefsLhs = globalDefsLhs + (s->x)
      println(s"val $x = $s")
      GRef(x)
    }
      //globalDefs.collect { case (k,`s`) => GRef(k) }.headOption getOrElse { 
      //val x = x0; globalDefs = globalDefs :+ (x->s); println(s"val $x = $s"); GRef(x) }
    def dreflect(s: Def): GVal = dreflect(freshVar,s)


    // *** input language Exp

    val IR = IRD

    type Val = IR.From
    type Var = String
    type Addr = String
    type Alloc = String
    type Field = String

    def vref(x: String): Val = IR.const(x)

    abstract class Exp
    case class Const(x: Any) extends Exp
    case class Direct(x: Val) extends Exp
    case class Ref(x: Var) extends Exp
    case class Assign(x: Var, y: Exp) extends Exp
    case class Plus(x: Exp, y: Exp) extends Exp
    case class Times(x: Exp, y: Exp) extends Exp
    case class Less(x: Exp, y: Exp) extends Exp
    case class New(x: Alloc) extends Exp
    case class Get(x: Exp, f: Exp) extends Exp
    case class Put(x: Exp, f: Exp, y: Exp) extends Exp
    case class If(c: Exp, a: Exp, b: Exp) extends Exp
    case class While(c: Exp, b: Exp) extends Exp
    case class Block(xs: List[Exp]) extends Exp {
      override def toString = "{\n  " + xs.map(_.toString).mkString("\n").replace("\n","\n  ") + "\n}"
    }

    // *** evaluator: Exp -> IR

    val store0 = IR.const(Map())
    val itvec0 = IR.const(1)

    var store: Val = store0
    var itvec: Val = itvec0

    def eval(e: Exp): Val = e match {
      case Const(x)    => IR.const(x)
      case Direct(x)   => IR.const(x)
      case Ref(x)      => IR.select(IR.select(store,IR.const("&"+x)), IR.const("val"))
      case Assign(x,y) => 
        store = IR.update(store, IR.const("&"+x), IR.update(IR.const(Map()), IR.const("val"), eval(y)))
        IR.const(())
      case Plus(x,y)   => IR.plus(eval(x),eval(y))
      case Times(x,y)  => IR.times(eval(x),eval(y))
      case Less(x,y)   => IR.less(eval(x),eval(y))
      case New(x) => 
        val a = IR.pair(IR.const(x),itvec)
        store = IR.update(store, a, IR.const(Map()))
        a
      case Get(x, f) => 
        IR.select(IR.select(store, eval(x)), eval(f))
      case Put(x, f, y) => 
        val a = eval(x)
        val old = IR.select(store, a)
        store = IR.update(store, a, IR.update(old, eval(f), eval(y)))
        IR.const(())
      case If(c,a,b) => 
        val c1 = eval(c)
        //if (!mayZero(c1)) eval(a) else if (mustZero(c1)) eval(b) else {
          val save = store
          //assert(c1)
          val e1 = eval(a)
          val s1 = store
          store = save
          //assertNot(c1)
          val e2 = eval(b)
          val s2 = store
          store = IR.iff(c1,s1,s2)
          IR.iff(c1,e1,e2)
        //}
      case While(c,b) =>  

        /*{println(s"def loop(store0, n0) = {")
        val savest = store
        store = "store0"
        val saveit = itvec
        itvec = itvec+"::n0"
        val c0x = eval(c)
        eval(b)
        println(s"if ($c0x) loop($store,n0+1) else (store0,n0)")
        println("}")
        store = savest
        itvec = saveit
        val n = reflect(s"loop($store,0)._2 // count")}*/

        import IR._
        val saveit = itvec

        val loop = GRef(freshVar)
        val n0 = GRef(freshVar)

        val before = store

        itvec = pair(itvec,n0)


      var init = before
      def iter: GVal = {

        def lub(a: GVal, b: GVal)(ploop: GVal) = (a,b) match {
          case (a,b) if a == b => a
          case _ => iff(less(const(0), n0), call(ploop,plus(n0,const(-1))), a)
        }

        store = iff(less(const(0), n0), call(loop,plus(n0,const(-1))), before)

        val cv = eval(c)

        val afterC = store

        store = subst(afterC,cv,const(1)) // assertTrue
        eval(b)

        val afterB = store

        val gen = lub(before, afterB)(loop)

        println(s"lub($before, $afterB) = $gen")
        if (init != gen) { init = gen; iter } else {

        // TODO: clarify intended semantics!
        // Is elem 0 the value after 0 iterations,
        // or the value computed in iteration 0?
        // The analogy of modeling values computed in
        // loops as arrays indexed by iteration would
        // suggest the meaning 'computed in iteration i'.
        // But then the value before the loop has index -1.
        // Need to investigate whether this is a problem.
        // It seems like we can avoid referring to -1
        // by proper index handling after the loop.

        // store at this point describes result *after* iteration i
        //  1 + (if (0<x) f(x-1) else 0)  =   if (0<x) f(x-1) + 1 else 1
        // but what we want for the function body:
        //  if (0<x) f(x-1) + 1 else 0
        // we rely on propagation of conditions to get there:

        store = iff(less(const(0), n0), store, before)

        // The alternative would be to make f(i) denote
        // the computed element in iteration i, and then pick
        // element n-1 after the loop.
        // It may seem unintuitive that f(i) = i+1 for a
        // simple counting loop and we might want to fix
        // it up with rewriting.
        // On the other hand, for dynamic allocations, 
        // we get f(i) = new A_i, which makes a lot of
        // sense.

        cv
        }
      }

        val cv = iter

        fun(loop.toString, n0.toString, store)

        val nX = fixindex(n0.toString, cv) // TODO: check this ...
        store = call(loop,nX)
        val cv1 = eval(c)
        store = subst(store,cv1,const(0)) // assertFalse

        itvec = saveit

/*
        VERSION (1) BELOW

          val savevc = varCount
          val savest = store
          val saveit = itvec

          // case i == 0
          store = savest
          itvec = IR.pair(itvec,IR.const(0))
          //val c0 = eval(c) to eval or not to eval?
          val afterC0 = store

          // case i > 0
          store = IR.call(loop,IR.plus(n0,IR.const(-1)))
          itvec = IR.pair(itvec,n0)

          val cX = eval(c)
          val afterCX = store
          eval(b)
          val afterBX = store
          val r0 = IR.iff(cX, afterBX, afterCX)
          val r = IR.iff(IR.less(IR.const(0),n0), r0, afterC0)

          store = savest
          itvec = saveit

        val xx = IR.fun(loop.toString, n0.toString, r)
        assert(xx === loop)

        val n = IR.fixindex(loop)
        store = IR.call(loop,n)

        eval(c) // once more (this one will fail)
*/
        
        IR.const(())

      case Block(Nil) => IR.const(())
      case Block(xs) => xs map eval reduceLeft ((a,b) => b)
    }


    // *** run and test

    def run(testProg: Exp) = {
      println("prog: " + testProg)
      store = store0
      itvec = itvec0
      varCount = varCount0
      globalDefs = globalDefs0
      rebuildGlobalDefsCache()
      val res = eval(testProg)
      println("res: " + res)
      println("store: " + store)
      val store2 = IR.iterateAll(store)
      println("transformed: " + store2)
      val sched = IR.schedule(store2)
      println("sched:")
      sched.foreach(IR.printStm)
      println("term:")
      IR.printTerm(store2)

      //store.printBounds
      println("----")
    }

    // test some integer computations

    val testProg1 = Block(List(
      Assign("i", Const(0)),
      Assign("y", Const(0)),
      Assign("x", Const(8)),
      While(Less(Ref("i"),Const(100)), Block(List(
        Assign("x", Const(7)),
        Assign("x", Plus(Ref("x"), Const(1))),
        Assign("y", Plus(Ref("y"), Const(1))),
        Assign("i", Plus(Ref("i"), Const(1)))
      )))
    ))

    val testProg2 = Block(List(
      Assign("x", Const(900)), // input
      Assign("y", Const(0)),
      Assign("z", Const(0)),
      While(Less(Const(0), Ref("x")), Block(List(
        Assign("z", Plus(Ref("z"), Ref("x"))),
        If(Less(Ref("y"),Const(17)), 
          Block(List(
            Assign("y", Plus(Ref("y"), Const(1)))
          )),
          Block(Nil)
        ),
        Assign("x", Plus(Ref("x"), Const(-1)))
      ))),
      Assign("r", Ref("x"))
    ))

    // test arrays / computed index access

    //   first, some unit tests
    val testProgUnit1 = Block(List(
      Assign("x", Const(0)), // "input"
      Assign("a", New("A")),
      Put(Ref("a"), Const("field"), Times(Ref("x"),Const(2))),
      Assign("r", Ref("a"))
    ))

    val testProgUnit2 = Block(List(
      Assign("x", Const(0)), // "input"
      Assign("a", New("A")),
      Put(Ref("a"), Ref("x"), Times(Ref("x"),Const(2))),
      Assign("r", Ref("a"))
    ))

    val testProg1b = Block(List(
      Assign("x", Const(0)),
      Assign("a", New("A")),
      Put(Ref("a"), Const("field"), Const(7)),
      While(Less(Ref("x"),Const(100)), Block(List(
        Put(Ref("a"), Const("field"), Const(7)),
        Assign("x", Plus(Ref("x"), Const(1)))
      ))),
      Assign("r", Ref("a"))
    ))

    //   update array at loop index
    val testProg1c = Block(List(
      Assign("x", Const(0)),
      Assign("y", Const(10)),
      Assign("a", New("A")), 
      // writes may not be visible if A is not in store at iteration 0
      Put(Ref("a"), Ref("x"), Times(Ref("x"),Const(2))),
      While(Less(Ref("x"),Const(100)), Block(List(
        Put(Ref("a"), Ref("x"), Times(Ref("x"),Const(2))),
        Assign("x", Plus(Ref("x"), Const(1))),
        Assign("y", Plus(Ref("y"), Const(1)))
      ))),
      Assign("r", Ref("a"))
    ))

    // test store logic

    val testProg3 = Block(List(
      Assign("i", Const(0)),
      Assign("z", New("A")),
      Assign("x", Ref("z")),
      While(Less(Ref("i"),Const(100)), Block(List(
        Assign("y", New("B")),
        Put(Ref("y"), Const("head"), Ref("i")),
        Put(Ref("y"), Const("tail"), Ref("x")),
        Assign("x", Ref("y")),
        Assign("i", Plus(Ref("i"), Const(1)))
      )))
    ))

    val testProg4 = Block(List(
      Assign("i", Const(0)),
      Assign("z", New("A")),
      Assign("x", Ref("z")),
      Assign("y", New("B")),
      While(Less(Ref("i"),Const(100)), Block(List(
        Put(Ref("y"), Const("head"), Ref("i")),
        Put(Ref("y"), Const("tail"), Ref("x")),
        Assign("x", Ref("y")),
        Assign("i", Plus(Ref("i"), Const(1)))
      )))
    ))

    val testProg5 = Block(List(
      Assign("i", Const(0)),
      Assign("z", New("A")),
      Assign("x", Ref("z")),
      While(Less(Ref("i"),Const(100)), Block(List(
        Put(Ref("x"), Const("head"), Ref("i")),
        Assign("i", Plus(Ref("i"), Const(1)))
      )))
    ))

    // modify stuff after a loop

    val testProg6 = Block(List(
      Assign("i", Const(0)),
      Assign("z", New("A")),
      Assign("x", Ref("z")),
      Assign("y", New("B")),
      While(Less(Ref("i"),Const(100)), Block(List(
        Put(Ref("y"), Const("head"), Ref("i")),
        Put(Ref("y"), Const("tail"), Ref("x")),
        Assign("x", Ref("y")),
        Assign("i", Plus(Ref("i"), Const(1)))
      ))),
      Put(Ref("y"), Const("tail"), Ref("z")),
      Put(Ref("y"), Const("head"), Const(7))
    ))

    // strong update for if

    val testProg7 = Block(List(
      Assign("x", New("A")),
      If(Direct(vref("input")),
        Block(List(
          Put(Ref("x"), Const("a"), New("B")),
          Put(Get(Ref("x"), Const("a")), Const("foo"), Const(5))
        )),
        Block(List(
          Put(Ref("x"), Const("a"), New("C")),
          Put(Get(Ref("x"), Const("a")), Const("bar"), Const(5))
        ))
      ),
      Assign("foo", Get(Get(Ref("x"), Const("a")), Const("foo"))),
      Assign("bar", Get(Get(Ref("x"), Const("a")), Const("bar")))
    ))

    val testProg8 = Block(List(
      Assign("x", New("A")),
      Put(Ref("x"), Const("a"), New("A2")),
      Put(Get(Ref("x"), Const("a")), Const("baz"), Const(3)),
      If(Direct(vref("input")),
        Block(List(
          Put(Ref("x"), Const("a"), New("B")), // strong update, overwrite
          Put(Get(Ref("x"), Const("a")), Const("foo"), Const(5))
        )),
        Block(List(
          Put(Ref("x"), Const("a"), New("C")), // strong update, overwrite
          Put(Get(Ref("x"), Const("a")), Const("bar"), Const(5))
        ))
      ),
      Put(Get(Ref("x"), Const("a")), Const("bar"), Const(7)), // this is not a strong update, because 1.a may be one of two allocs
      Assign("xbar", Get(Get(Ref("x"), Const("a")), Const("bar"))) // should still yield 7!
    ))

    // update stuff allocated in a loop

    val testProg9 = Block(List(
      Assign("x", New("X")),
      Put(Ref("x"), Const("a"), New("A")),
      Put(Get(Ref("x"), Const("a")), Const("baz"), Const(3)),
      While(Direct(vref("input")),
        Block(List(
          Put(Ref("x"), Const("a"), New("B")), // strong update, overwrite
          Put(Get(Ref("x"), Const("a")), Const("foo"), Const(5))
        ))
      ),
      Put(Get(Ref("x"), Const("a")), Const("bar"), Const(7)), // this is not a strong update, because 1.a may be one of two allocs
      Assign("xbar", Get(Get(Ref("x"), Const("a")), Const("bar"))) // should still yield 7!
    ))

  }

  def testA = withOutFileChecked(prefix+"A") {
    Test1.run(Test1.testProg1)
    Test1.run(Test1.testProg2)
  }

  def testA2 = withOutFileChecked(prefix+"A2") {
    Test1.run(Test1.testProgUnit1)
    Test1.run(Test1.testProgUnit2)
  }

  def testA3 = withOutFileChecked(prefix+"A3") {
    Test1.run(Test1.testProg1b)
  }

  def testA4 = withOutFileChecked(prefix+"A4") {
    Test1.run(Test1.testProg1c)
  }


  def testB = withOutFileChecked(prefix+"B") {
    Test1.run(Test1.testProg3)
//    Test1.run(Test1.testProg4) // 3 and 4 should be different: alloc within the loop vs before
//    Test1.run(Test1.testProg5)
  }
  /*def testC = withOutFileChecked(prefix+"C") {
    Test1.run(Test1.testProg6)
  }
  def testD = withOutFileChecked(prefix+"D") {
    Test1.run(Test1.testProg7)
    Test1.run(Test1.testProg8)
  }
  def testE = withOutFileChecked(prefix+"E") {
    Test1.run(Test1.testProg9)
  }*/



}