package lancet
package analysis

class TestAnalysis4 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-4"

/* 
  make loop ranges explicit, reason about
  an infinite number of memory addresses
  (allocation site indexed by loop iteration)

  TODO -- UNFINISHED
*/

/*



*/

  object Test1 {

    type Var = String
    type Addr = String
    type Alloc = String
    type Field = String


    type Val = String

    //val store0: Store = Store(Map.empty, Map.empty, Set.empty, Set.empty)
    //var store: Store = _

    //val itvec0 = List(1)
    //var loopDepth = loopDepth0

    abstract class Exp
    case class Const(x: Int) extends Exp
    case class Direct(x: Val) extends Exp
    case class Ref(x: Var) extends Exp
    case class Assign(x: Var, y: Exp) extends Exp
    case class Plus(x: Exp, y: Exp) extends Exp
    case class Less(x: Exp, y: Exp) extends Exp
    case class New(x: Alloc) extends Exp
    case class Get(x: Exp, f: Field) extends Exp
    case class Put(x: Exp, f: Field, y: Exp) extends Exp
    case class If(c: Exp, a: Exp, b: Exp) extends Exp
    case class While(c: Exp, b: Exp) extends Exp
    case class Block(xs: List[Exp]) extends Exp {
      override def toString = "{\n  " + xs.map(_.toString).mkString("\n").replace("\n","\n  ") + "\n}"
    }

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


    def vref(x: String): Val = x

    val varCount0 = 0
    var varCount = varCount0

    def reflect(s: String): String = { println(s"val x$varCount = $s"); varCount += 1; s"x${varCount-1}" }
    def reify(x: => String): String = captureOutputResult(x)._1

    val store0 = s"Map()"
    val itvec0 = s"1"

    var store = store0
    var itvec = itvec0

    def eval(e: Exp): String = e match {
      case Const(x)    => s"$x"
      case Direct(x)   => s"$x"
      case Ref(x)      => reflect(s"$store(&$x).val")
      case Assign(x,y) => 
        store = reflect(s"$store + (&$x -> Map(val -> ${eval(y)}))")
        "()"
      case Plus(x,y)   => reflect(s"${eval(x)} + ${eval(y)}")
      case Less(x,y)   => reflect(s"${eval(x)} < ${eval(y)}")
      case New(x) => 
        val a = s"${x}_$itvec"
        store = reflect(s"$store + ($a -> Map())")
        a
      case Get(x, f) => 
        reflect(s"$store(${eval(x)}).$f")
      case Put(x, f, y) => 
        store = reflect(s"$store + ($store(${eval(x)}) + ($f -> ${eval(y)}))")
        "()"
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
          store = reflect(s"if ($c1) $s1 else $s2")
          reflect(s"if ($c1) $e1 else $e2")
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


        {println(s"def loop(n0: Int): (Int,Store) = {")
        val savevc = varCount
        val savest = store
        val saveit = itvec
        val prev = reflect(s"if (n0 <= 0) (0,$savest) else loop(n0-1)")
        store = s"$prev._2"
        itvec = itvec+"::n0"
        val c0x = eval(c)
        val afterC = store
        eval(b)
        println(s"if ($c0x) ($prev._1+1,$store) else ($prev._1,$afterC)")
        println("}")
        store = savest
        itvec = saveit
        val n = reflect(s"fixindex(loop)")
        store = reflect(s"loop($n)._2")}

        // TODO: fixpoint

        "()"

      case Block(Nil) => "()"
      case Block(xs) => xs map eval reduceLeft ((a,b) => b)
    }




    def run(testProg: Exp) = {
      println("prog: " + testProg)
      store = store0
      // /loopDepth = loopDepth0
      val res = eval(testProg)
      println("res: " + res)
      println(store)
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
        Assign("y", Plus(Ref("y"), Const(1))), // TOOD: how to relate to loop var??
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

    // test store logic

    val testProg3 = Block(List(
      Assign("i", Const(0)),
      Assign("z", New("A")),
      Assign("x", Ref("z")),
      While(Less(Ref("i"),Const(100)), Block(List(
        Assign("y", New("B")),
        Put(Ref("y"), "head", Ref("i")),
        Put(Ref("y"), "tail", Ref("x")),
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
        Put(Ref("y"), "head", Ref("i")),
        Put(Ref("y"), "tail", Ref("x")),
        Assign("x", Ref("y")),
        Assign("i", Plus(Ref("i"), Const(1)))
      )))
    ))

    val testProg5 = Block(List(
      Assign("i", Const(0)),
      Assign("z", New("A")),
      Assign("x", Ref("z")),
      While(Less(Ref("i"),Const(100)), Block(List(
        Put(Ref("x"), "head", Ref("i")),
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
        Put(Ref("y"), "head", Ref("i")),
        Put(Ref("y"), "tail", Ref("x")),
        Assign("x", Ref("y")),
        Assign("i", Plus(Ref("i"), Const(1)))
      ))),
      Put(Ref("y"), "tail", Ref("z")),
      Put(Ref("y"), "head", Const(7))
    ))

    // strong update for if

    val testProg7 = Block(List(
      Assign("x", New("A")),
      If(Direct(vref("input")),
        Block(List(
          Put(Ref("x"), "a", New("B")),
          Put(Get(Ref("x"), "a"), "foo", Const(5))
        )),
        Block(List(
          Put(Ref("x"), "a", New("C")),
          Put(Get(Ref("x"), "a"), "bar", Const(5))
        ))
      ),
      Assign("foo", Get(Get(Ref("x"), "a"), "foo")),
      Assign("bar", Get(Get(Ref("x"), "a"), "bar"))
    ))

    val testProg8 = Block(List(
      Assign("x", New("A")),
      Put(Ref("x"), "a", New("A2")),
      Put(Get(Ref("x"), "a"), "baz", Const(3)),
      If(Direct(vref("input")),
        Block(List(
          Put(Ref("x"), "a", New("B")), // strong update, overwrite
          Put(Get(Ref("x"), "a"), "foo", Const(5))
        )),
        Block(List(
          Put(Ref("x"), "a", New("C")), // strong update, overwrite
          Put(Get(Ref("x"), "a"), "bar", Const(5))
        ))
      ),
      Put(Get(Ref("x"), "a"), "bar", Const(7)), // this is not a strong update, because 1.a may be one of two allocs
      Assign("xbar", Get(Get(Ref("x"), "a"), "bar")) // should still yield 7!
    ))

    // update stuff allocated in a loop

    val testProg9 = Block(List(
      Assign("x", New("X")),
      Put(Ref("x"), "a", New("A")),
      Put(Get(Ref("x"), "a"), "baz", Const(3)),
      While(Direct(vref("input")),
        Block(List(
          Put(Ref("x"), "a", New("B")), // strong update, overwrite
          Put(Get(Ref("x"), "a"), "foo", Const(5))
        ))
      ),
      Put(Get(Ref("x"), "a"), "bar", Const(7)), // this is not a strong update, because 1.a may be one of two allocs
      Assign("xbar", Get(Get(Ref("x"), "a"), "bar")) // should still yield 7!
    ))

  }

  def testA = withOutFileChecked(prefix+"A") {
    Test1.run(Test1.testProg1)
    Test1.run(Test1.testProg2)
  }

  def testB = withOutFileChecked(prefix+"B") {
    Test1.run(Test1.testProg3)
    Test1.run(Test1.testProg4) // 3 and 4 should be different: alloc within the loop vs before
    Test1.run(Test1.testProg5)
  }
  def testC = withOutFileChecked(prefix+"C") {
    Test1.run(Test1.testProg6)
  }
  def testD = withOutFileChecked(prefix+"D") {
    Test1.run(Test1.testProg7)
    Test1.run(Test1.testProg8)
  }
  def testE = withOutFileChecked(prefix+"E") {
    Test1.run(Test1.testProg9)
  }



}