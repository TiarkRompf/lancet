package lancet
package interpreter

import lancet.api._

class TestInterpreter3 extends BaseTestInterpreter3 {
  val prefix = "test-out/test-interpreter-3"
  def newCompiler = new BytecodeInterpreter_TIR_Opt with Compiler {
    initialize()
    debugBlockKeys = false
  }
}

class TestInterpreter3LMS extends BaseTestInterpreter3 {
  val prefix = "test-out/test-interpreter-3-LMS-"
  def newCompiler = new BytecodeInterpreter_LMS_Opt with Compiler {
    initialize()
    debugBlockKeys = false
    def compile[A:Manifest,B:Manifest](f: A => B): A=>B = compile0(f)
  }
}

trait Compiler {
  def compile[A:Manifest,B:Manifest](f: A => B): A=>B
}

trait BaseTestInterpreter3 extends FileDiffSuite {

  def newCompiler: Compiler
  val prefix: String

  final class Bar {
    var intField: Int = _
    var objField: AnyRef = _
  }

  // test the following:
  // - reads/writes on allocs and constants
  // - conditionals and loops


  // dynamically allocated object

  def testA1 = withOutFileChecked(prefix+"A1") {
    val it = newCompiler
    val f = it.compile { (x:Int) => 
        val b = new Bar
        b.intField = 7
        if (x > 0) {
          b.intField = 9
        }
        b.intField
    }
    printcheck(f(7), 9)
  }

  def testA2 = withOutFileChecked(prefix+"A2") {
    val it = newCompiler
    val f = it.compile { (x:Int) => 
        val b = new Bar
        b.intField = 7
        var y = x
        while (y > 0) {
          b.intField += 1
          y -= 1
        }
        b.intField
    }
    printcheck(f(7), 14)
  }

  def testA3 = withOutFileChecked(prefix+"A3") {
    val it = newCompiler
    val f = it.compile { (x:Int) => 
        val b = new Bar
        b.intField = 7
        if (x > 0) {
          b.intField = 9
        }
        var y = 0
        while (b.intField > 0) {
          b.intField -= 1
          y += 1
        }
        y
    }
    printcheck(f(7), 9)
  }

  // static object

  def testB1 = withOutFileChecked(prefix+"B1") {
    val it = newCompiler
    val b = new Bar
    b.intField = 7
    val f = it.compile { (x:Int) => 
        if (x > 0) {
          b.intField = 9
        }
        b.intField
    }
    printcheck(f(7), 9)
  }

  def testB2 = withOutFileChecked(prefix+"B2") {
    val it = newCompiler
    val b = new Bar
    b.intField = 7
    val f = it.compile { (x:Int) => 
        var y = x
        while (y > 0) {
          b.intField += 1
          y -= 1
        }
        b.intField
    }
    printcheck(f(7),14)
  }

  def testB3 = withOutFileChecked(prefix+"B3") {
    val it = newCompiler
    val b = new Bar
    b.intField = 7
    val f = it.compile { (x:Int) => 
        if (x > 0) {
          b.intField = 9
        }
        var y = 0
        while (b.intField > 0) {
          b.intField -= 1
          y += 1
        }
        y
    }
    printcheck(f(7),9)
  } 


/* 
  test that needs multiple speculative iterations:

  var x = 0
  var y = 0
  while (x < 10) {
    if (x > 0) {
      y += 1
    }
    x += 1

  }

  iteration 0:

  before: x = 0, y = 0
  after : x = 1, y = 0

  generalize x

  iteration 1:

  before: x = ?, y = 0
  after : x = ?, y = ?

  generalize y

  (do we need iteration 2 or can we just take result as gen?)
*/
}