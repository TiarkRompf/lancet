package lancet
package interpreter

import lancet.api._

class TestInterpreter6 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-6"

  def test1 = withOutFileChecked(prefix+"stable1") {
    val it = new Decompiler

    def compute(i: Int) = stable(i >= 50)

    val f = it.compile { (x:Int) => 
      var i = 0
      while (i < x) {
        compute(i)
        i += 1
      }
      i
    }
    printcheck(f(100), 100)
  }

  def test2 = withOutFileChecked(prefix+"stable2") {
    val it = new Decompiler

    @stable var fiftyOrMore = false

    def compute(i: Int) = fiftyOrMore = (i >= 50)

    val f = it.compile { (x:Int) => 
      var i = 0
      while (i < x) {
        compute(i)
        i += 1
      }
      i
    }
    printcheck(f(100), 100)
  }



  def xxtest10 = withOutFileChecked(prefix+"language10") {
    assert(false)
    val it = new Decompiler

    abstract class Exp
    case class IntVal(x: Int) extends Exp
    case class DoubleVal(x: Double) extends Exp
    case class StringVal(x: String) extends Exp
    case class Plus(x: Exp, y: Exp) extends Exp
    case class Minus(x: Exp, y: Exp) extends Exp
    case class Var(x: String) extends Exp
    
    abstract class Stm
    case class Assign(x: String, y: Exp) extends Stm
    case class While(x: Exp, y: Stm) extends Stm
    case class Print(x: Exp) extends Stm
    case class Block(xs: List[Stm]) extends Stm

    val env = new scala.collection.mutable.HashMap[String,Any]
    def eval(e: Exp): Any = e match {
      case IntVal(x) => x
      case DoubleVal(x) => x
      case StringVal(x) => x
      case Plus(x,y) => (eval(x),eval(y)) match {
        case (x: Int, y: Int) => x + y
        case (x: Double, y: Double) => x + y
        case (x: String, y: String) => x + y 
        // todo widen
      }
      case Minus(x,y) => (eval(x),eval(y)) match {
        case (x: Int, y: Int) => x + y
        case (x: Double, y: Double) => x + y
        case (x: String, y: String) => x + y 
        // todo widen
      }
      case Var(x) => env(x)
    }
    def exec(s: Stm): Unit = s match {
      case Assign(x,y) => env(x) = eval(y)
      case While(x,y) => while (eval(x).asInstanceOf[Int] > 0) exec(y) // test int
      case Print(x) => println(eval(x))
      case Block(xs) => xs.foreach(exec(_)) // unroll
    }

    // GOAL 1: switch plus/minus nodes
    // GOAL 2: switch environment if new keys added

    val p = Block(List(
      //Assign("max", IntVal(100)),
      Assign("x", IntVal(0)),
      While(Minus(Var("max"), Var("x")), Block(List(
        Print(Var("x")),
        Assign("x", Plus(Var("x"), IntVal(1)))
      )))
    ))

    val f = it.compile { (x:Int) => 
      env("max") = x
      exec(p)
    }
    println(f(100))
  }





  class Decompiler extends BytecodeInterpreter_LIR_Opt with DefaultMacros {

    initialize()
    emitUniqueOpt = true
    debugBlockKeys = false
    debugReadWrite = false
    debugMethods = false
    debugReturns = true

  }

}
