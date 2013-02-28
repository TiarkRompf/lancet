package lancet
package analysis

class TestAnalysis2 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-2"

/* 
  making fixpoints explicit in the ai lattice
  test integer ranges

*/

  object Test1 {
    // concrete evaluation semantics

    type Var = String

    abstract class Val
    case class VConst(x: Int) extends Val{
      override def toString = x.toString
    }
    case class VRef(x: Var) extends Val {
      override def toString = x
    }
    case class VLess(a: Val, b: Val) extends Val {
      override def toString = a+"<"+b
    }
    case class VPlus(a: Val, b: Val) extends Val {
      override def toString = a+"+"+b
    }
    case class VIf(c: Val, a: Val, b: Val) extends Val {
      override def toString = "phi("+a+","+b+")"
    }
    case class VWhile(c: Val, a: Val, b: Val) extends Val {
      override def toString = "lphi("+a+","+b+")"
    }

    def vconst(x: Int) = VConst(x)
    def vref(x: Var) = VRef(x)
    def vless(a: Val, b: Val) = (a,b) match {
      case (VConst(a),VConst(b)) => VConst(if (a < b) 1 else 0)
      case _ => VLess(a,b)
    }
    def vplus(a: Val, b: Val) = (a,b) match {
      case (VConst(a),VConst(b)) => VConst(a+b)
      case _ => VPlus(a,b)
    }
    def vif(c: Val, a: Val, b: Val) = if (a == b) a else VIf(c,a,b)
    def vwhile(c: Val, a: Val, b: Val) = if (a == b) a else VWhile(c,a,b)




    case class Store(m: Map[Var,Val]) {
      def apply(x:Var) = m(x)
      def +(x:(Var,Val)) = Store(m+x)
      override def toString = m.mkString("\n")
      def bound = 
    }

    def infix_join(a: Store, b: Store): Store = {
      val m = (a.m.keys ++ b.m.keys).map { k => (k, (a.m.get(k),b.m.get(k)) match {
        case (Some(a),Some(b)) if a == b => a
        case (Some(a),Some(b)) => vif(vconst(-2), a, b)
        //case (Some(a),_) => a
        //case (_,Some(b)) => b
      })}.toMap
      Store(m)
    }

    def infix_joinFix(a: Store, b: Store): Store = {
      val m = (a.m.keys ++ b.m.keys).map { k => (k, (a.m.get(k),b.m.get(k)) match {
        case (Some(a),Some(b)) if a == b => a
        case (Some(a),Some(b)) => println(k+"0="+a+" or "+b); vref(k+"0")
        //case (Some(a),_) => a
        //case (_,Some(b)) => b
      })}.toMap
      Store(m)
    }

    def infix_join(a: Val, b: Val): Val  = vif(vconst(-3),a,b)


    def mayZero(a: Val): Boolean = true
    def mustZero(a: Val): Boolean = false


    abstract class Exp
    case class Const(x: Int) extends Exp
    case class Ref(x: Var) extends Exp
    case class Assign(x: Var, y: Exp) extends Exp
    case class Plus(x: Exp, y: Exp) extends Exp
    case class Less(x: Exp, y: Exp) extends Exp
    case class If(c: Exp, a: Exp, b: Exp) extends Exp
    case class While(c: Exp, b: Exp) extends Exp
    case class Block(xs: List[Exp]) extends Exp {
      override def toString = "{\n  " + xs.map(_.toString).mkString("\n").replace("\n","\n  ") + "\n}"
    }

    var store: Store = Store(Map.empty)

    def eval(e: Exp): Val = e match {
      case Const(x) => vconst(x)
      case Ref(x) => store(x)
      case Assign(x,y) => store = store + (x -> eval(y)); vconst(-666)
      case Plus(x,y) => vplus(eval(x),eval(y))
      case Less(x,y) => vless(eval(x),eval(y))
      case If(c,a,b) => 
        val c1 = eval(c)
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
      case While(c,b) => 
        eval(c)
        val sBefore0 = store
        eval(b)
        eval(c)
        val sAfter0 = store
        val sBefore1 = sBefore0 joinFix sAfter0
        store = sBefore1
        eval(b)
        eval(c)
        val sAfter1 = store
        store = sBefore0 join sAfter1
        vconst(-668)
      case Block(Nil) => vconst(-667)
      case Block(xs) => xs map eval reduceLeft ((a,b) => b)
    }


    val testProg1 = Block(List(
      Assign("i", Const(0)),
      Assign("y", Const(0)),
      Assign("x", Const(8)),
      While(Less(Ref("i"),Const(100)), Block(List(
        Assign("x", Const(7)),
        Assign("x", Plus(Ref("x"), Const(1))),
        Assign("y", Plus(Ref("i"), Const(1))),
        Assign("i", Plus(Ref("i"), Const(1)))
      )))
    ))


    def run(testProg: Exp) = {
      println("prog: " + testProg)
      val res = eval(testProg)
      println("res: " + res)
      println("store: \n" + store)
    }

  }


  // run it
  def testA = withOutFileChecked(prefix+"A") {
    Test1.run(Test1.testProg1)
  }


}