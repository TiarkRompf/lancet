package lancet
package interpreter

import lancet.api._

class TestInterpreter6 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-6"


  def test1 = withOutFileChecked(prefix+"stable1") {
    val it = new Decompiler

    def stable(x: => Int): Int = {
      val c = it.freeze(x)
      val y = x
      if (c != y) { it.fastpath; y }
      else c
    }


    var fiftyOrMore = 99

    def compute(i: Int) = {
      val c = it.freeze[Int](fiftyOrMore)
      if (c != fiftyOrMore) 
        it.fastpath()

      print("static:  ");    println(c)
      print("dynamic: ");    println(fiftyOrMore)

      fiftyOrMore = if (i < 50) 0 else 1

      7
    }

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

  /*def test2 = withOutFileChecked(prefix+"stable2") {
    val it = new Decompiler

    class Holder {
      @stable var fiftyOrMore = false
    }
    val Holder = new Holder

    def compute(i: Int) = Holder.fiftyOrMore = (i >= 50)

    val f = it.compile { (x:Int) => 
      var i = 0
      while (i < x) {
        compute(i)
        i += 1
      }
      i
    }
    printcheck(f(100), 100)
  }*/




  class Decompiler extends BytecodeInterpreter_LIR_Opt with DefaultMacros {

    initialize()
    emitUniqueOpt = true
    debugBlockKeys = false
    debugReadWrite = false
    debugMethods = false
    debugReturns = true

  }

}
