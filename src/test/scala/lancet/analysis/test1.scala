package lancet
package analysis

class TestAnalysis1 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-1"

/* 
    order of optimizations
      1) rewrite to constant (3+1 --> 4; x*0 --> 0)
      2) rewrite to previously defined value (x+0 --> x; x+y-y --> x)
      3) don't perform operation twice (cse)


    generalizing the store abstraction

    requirements:
      1) forward analysis to shortcut and eliminate
         reads (propagate values through data structures)
      2) generate dependencies between statements,
         so that they can be soundly moved around
         and dce'd (enable backwards analysis on dep 
         graph)

    concrete semantics:

      Frame = Local -> Val (Frame not used here)
      Val   = Prim Int | Ref Addr
      Obj   = Class x Field -> Val
      Store = Addr -> Obj

      eval: Exp => Val
      e[ new C ]            =   { val a        = freshAddr
                                  store        = store update [a -> (C, fields(C) -> null))]
                                  a }
      e[ x.f = y ]          =   { val Ref a    = eval(x)
                                  val y'       = eval(y) 
                                  val C,fields = store(a)
                                  store        = store update [a -> (C, fields update [f -> y'])]
                                  () }
      e[ x.f ]              =   { val Ref a    = eval(x)
                                  val C,fields = store(a)
                                  fields(f) }
      e[ if (c) a else b ]  =   { val Prim c'  = eval(x)
                                  if (c' != 0) eval(a) else eval(b) }

      (threading of store elided)


    collecting/abstract semantics:

      e[ if (c) a else b ]  =   { val Prim c' = eval(x)
                                  if (!mayZero(x')) 
                                    eval(a) 
                                  else if (mustZero(c')) 
                                    eval(b) 
                                  else eval(a) join eval(b) }

      Val^   = Prim Int^ | Ref Addr^
      Obj^   = Field -> Val^
      Store^ = Addr -> Obj^

      so: need to define Int^ and Addr^

        Int^:  mustZero, mayZero
        Addr^: 

      store.apply and store.update now
      need to take Addr^ objects

      need to precise what to update: we want
      strong updates -- only possible if we know
      that the target is a singleton

*/

  object Test1 {

    type Addr = Int
    type Field = String

    abstract class Val
    case class Prim(x: Int) extends Val
    case class Ref(x: Addr) extends Val

    type Obj = Map[Field,Val]
    type Store = Map[Addr,Obj]

    abstract class Exp
    case class Const(x: Int) extends Exp
    case class Static(x: Int) extends Exp
    case class New() extends Exp
    case class Get(x: Exp, f: Field) extends Exp
    case class Put(x: Exp, f: Field, y: Exp) extends Exp
    case class If(c: Exp, a: Exp, b: Exp) extends Exp
    case class Block(xs: List[Exp]) extends Exp {
      override def toString = "{\n  " + xs.map(_.toString).mkString("\n").replace("\n","\n  ") + "\n}"
    }

    var store: Store = Map(0 -> Map.empty)

    var curAddr = 100
    def freshAddr() = { try curAddr finally curAddr += 1 }

    def eval(e: Exp): Val = e match {
      case Const(x) => Prim(x)
      case Static(x) => Ref(x)
      case If(c,a,b) => 
        val Prim(c1) = eval(c)
        if (c1 != 0) eval(a) else eval(b)
      case Block(xs) => xs map eval reduceLeft ((a,b) => b)
      case New() => 
        val a = freshAddr()
        store = store + (a -> Map.empty)
        Ref(a)
      case Get(x, f) => 
        val Ref(a) = eval(x)
        store(a)(f)
      case Put(x, f, y) => 
        val Ref(a) = eval(x)
        val y1 = eval(y)
        val x1 = store.getOrElse(a, Map.empty)
        store = store + (a -> (x1 + (f -> y1)))
        Prim(0)
    }


    val testProg1 = Block(List(
      Put(Static(0), "counter", Const(1)),
      If(Get(Static(0), "counter"),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "foo", Const(5))
        )),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "bar", Const(5))
        ))
      )
    ))

    def run() = {
      println("prog: " + testProg1)
      val res = eval(testProg1)
      println("res: " + res)
      println("store: " + store)
    }

  }

  object Test2 {

    type Addr = Int
    type Field = String

    type AbsInt = Set[Int]
    type AbsAddr = Set[Addr]

    def mayZero(x: AbsInt) = x contains 0
    def mustZero(x: AbsInt) = x == Set(0)

    abstract class Val {
      def join(that: Val) = (this,that) match {
        case (Prim(x), Prim(y)) => Prim(x ++ y)
        case (Ref(x), Ref(y)) => Ref(x ++ y)
      }
    }
    case class Prim(x: AbsInt) extends Val
    case class Ref(x: AbsAddr) extends Val

    type Obj = Map[Field,Val]
    type Store = Map[Addr,Obj]

    def objJoin(a: Obj, b: Obj): Obj = {
      ???
    }
    def objJoin(os: List[Obj]): Option[Obj] =
      if (os.isEmpty) None else Some(os.reduceLeft(objJoin))

    abstract class AbsStore { self =>
      override def toString = List.range(0,100).flatMap(a=>get(Set(a)).map(a -> _)).toMap.toString
      def getOrElse(a: AbsAddr, b: => Obj): Obj = get(a).getOrElse(b)
      def apply(a: AbsAddr): Obj = get(a).get
      def get(a: AbsAddr): Option[Obj]
      def +(p: (AbsAddr,Obj)): AbsStore = join(AbsStore(p._1.map(_ -> p._2).toMap))
      def join(other: AbsStore): AbsStore = new AbsStore {
        def get(a: AbsAddr): Option[Obj] =
          objJoin(self.get(a).toList ++ other.get(a).toList)
      }
    }

    object AbsStore {
      def apply(x: Store) = new AbsStore {
        def get(a: AbsAddr): Option[Obj] =
          objJoin(a.toList.flatMap(x.get(_).toList))
      }
    }


    abstract class Exp
    case class Const(x: Int) extends Exp
    case class Static(x: Int) extends Exp
    case class New() extends Exp
    case class Get(x: Exp, f: Field) extends Exp
    case class Put(x: Exp, f: Field, y: Exp) extends Exp
    case class If(c: Exp, a: Exp, b: Exp) extends Exp
    case class Block(xs: List[Exp]) extends Exp {
      override def toString = "{\n  " + xs.map(_.toString).mkString("\n").replace("\n","\n  ") + "\n}"      
    }

    var store: AbsStore = AbsStore(Map(0 -> Map("counter" -> Prim(Set(0,1,2,3,4,5,6,7,8,9)))))

    var curAddr = 100
    def freshAddr() = { try Set(curAddr) finally curAddr += 1 }

    def eval(e: Exp): Val = e match {
      case Const(x) => Prim(Set(x))
      case Static(x) => Ref(Set(x))
      case If(c,a,b) => 
        val Prim(c1) = eval(c)
        println("c "+c1)
        if (!mayZero(c1)) eval(a) else if (mustZero(c1)) eval(b) else {
          val save = store
          val e1 = eval(a)
          val s1 = store
          store = save
          val e2 = eval(b)
          val s2 = store
          store = s1 join s2
          e1 join e2
        }
      case Block(xs) => xs map eval reduceLeft ((a,b) => b)
      case New() => 
        val a = freshAddr()
        store = store + (a -> Map.empty)
        Ref(a)
      case Get(x, f) => 
        val Ref(a) = eval(x)
        store(a)(f)
      case Put(x, f, y) => 
        val Ref(a) = eval(x)
        val y1 = eval(y)
        val x1 = store.getOrElse(a, Map.empty)
        store = store + (a -> (x1 + (f -> y1)))
        Prim(Set(0))
    }


    val testProg1 = Block(List(
      //Put(Static(0), "counter", Const(1)),
      If(Get(Static(0), "counter"),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "foo", Const(5))
        )),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "bar", Const(5))
        ))
      )
    ))

    def run() = {
      println("prog: " + testProg1)
      val res = eval(testProg1)
      println("res: " + res)
      println("store: " + store)
    }
  }



  // run it
  def testA = withOutFileChecked(prefix+"A") {
    Test1.run()
  }
  def testB = withOutFileChecked(prefix+"B") {
    Test2.run()
  }


}