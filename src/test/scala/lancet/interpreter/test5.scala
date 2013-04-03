package lancet
package interpreter

import lancet.api._

class TestInterpreter5 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-5"

  // TODO: delimit scope of slowpath/fastpath

  def test1a = withOutFileChecked(prefix+"slowpath1a") {
    val it = new Decompiler

    def compute(i: Int) = if (i == 50) it.slowpath

    val f = it.interpret { (x:Int) => // fully interpreted
      var i = 0
      while (i < x) {
        compute(i)
        i += 1
      }
      i
    }
    printcheck(f(100), 100)
  }

  def test1 = withOutFileChecked(prefix+"slowpath1") {
    val it = new Decompiler

    def compute(i: Int) = if (i == 50) it.slowpath

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

  def testFastpath1 = withOutFileChecked(prefix+"fastpath1") {
    val it = new Decompiler

    def compute(i: Int) = if (i == 50) it.fastpath

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


  def testFastpath2 = withOutFileChecked(prefix+"fastpath2") {
    val it = new Decompiler

    def compute(i: Int) = if (i % 10 == 0) it.fastpath

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

  def testCps1 = withOutFileChecked(prefix+"cps1") {
    val it = new Decompiler

    def compute(i: Int) = it.shift[Int,Int](k => k(7) + k(9))

    val f = it.compile { (x:Int) => 
      it.reset[Int](compute(x) * 100)
    }
    printcheck(f(100), 1600)
  }


  def test1LMS = withOutFileChecked(prefix+"slowpath1LMS") {
    val it = new DecompilerLMS

    def compute(i: Int) = if (i == 50) it.slowpath

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

  def testFastpath1LMS = withOutFileChecked(prefix+"fastpath1LMS") {
    val it = new DecompilerLMS

    def compute(i: Int) = if (i == 50) it.fastpath

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


  def testFastpath2LMS = withOutFileChecked(prefix+"fastpath2LMS") {
    val it = new DecompilerLMS

    def compute(i: Int) = if (i % 10 == 0) it.fastpath

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





  def testCps1LMS = withOutFileChecked(prefix+"cps1LMS") {
    val it = new DecompilerLMS

    def compute(i: Int) = it.shift[Int,Int](k => k(7) + k(9))

    val f = it.compile { (x:Int) => 
      it.reset[Int](compute(x) * 100)
    }
    printcheck(f(100), 1600)
  }


  def test11 = withOutFileChecked(prefix+"tree11") {
    val it = new Decompiler
    it.emitCheckCast = false
    it.emitRecursive = true
    it.debugReturns = false

    class Tree {
      var _key: Int = 0
      var _value: Double = 0
      var _left: Tree = null
      var _right: Tree = null

      def key = it.freezeInt(_key)
      def value = it.freezeInt(_key) // type!
      def left = it.freeze(_left)
      def right = it.freeze(_right)

      def put(key: Int, value: Double): Unit = {
        val l = left
        val r = right
        val k = this.key

        if (key == k) this._value += value // invalidate?
        else if (key < k) {
          if (l == null) {
            val t = new Tree
            t._key = key
            t._value = value
            _left = t // invalidate?
          }
          else l.put(key,value)
        } else {//if (key > this.key) {
          if (r == null) {
            val t = new Tree
            t._key = key
            t._value = value
            _right = t // invalidate?
          }
          else r.put(key,value)
        }
      }

      def get(key: Int): Double = {
        val l = left
        val r = right
        val k = this.key

        if (key == k) this.value
        else if (key < k) {
          if (l == null) -1d
          else l.get(key)
        } else {//if (key > this.key) {
          if (r == null) -1d
          else r.get(key)
        }
      }

    }


    val t1 = new Tree
    t1._key = 5
    t1._value = 5.0
    t1._left = new Tree
    t1._left._key = 2
    t1._left._value = 2.0
    t1._right = new Tree
    t1._right._key = 7
    t1._right._value = 7.0


    val f = it.compile { x: Int =>
      var i = 0
      while (i < 10) {
        t1.put(i,1.0)
        println(t1.get(i))
        i += 1
      }


      9
    }
    f(0)
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




  class DecompilerLMS extends BytecodeInterpreter_LMS_Opt with DefaultMacrosLMS {

    //def fastpath() = {} // XXX

    initialize()
    emitUniqueOpt = true
    debugBlockKeys = false
    debugReadWrite = false
    //debugMethods = false
    //debugReturns = true

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
