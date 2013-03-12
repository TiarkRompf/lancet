package lancet
package interpreter

import lancet.api._

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

class TestInterpreter3LMS extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-3-LMS-"

  final class Bar {
    var intField: Int = _
    var objField: AnyRef = _
  }


  // should test the following:
  // - reads/writes on allocs and constants
  // - loops
  // - exceptions
  // - redundant branches


  // dynamically allocated object

  def testA1 = withOutFileChecked(prefix+"A1") {
    val it = new BytecodeInterpreter_LMS_Opt
    it.initialize()
    val f = it.compile { (x:Int) => 
        val b = new Bar
        b.intField = 7
        if (x > 0) {
          b.intField = 9
        }
        b.intField
    }
    val y = f(7)
    println(y)
    assert(y == 9)
  }

  def testA2 = withOutFileChecked(prefix+"A2") {
    System.setProperty("lms.verbosity","1")
    val it = new BytecodeInterpreter_LMS_Opt
    it.initialize()
    it.debugDepGraph = true
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
    val y = f(7)
    println(y)
    assert(y == 14)
  }

  def testA3 = withOutFileChecked(prefix+"A3") {
    val it = new BytecodeInterpreter_LMS_Opt
    it.initialize()
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
    val y = f(7)
    println(y)
    assert(y == 9)
  }

  // static object

  def testB1 = withOutFileChecked(prefix+"B1") {
    val it = new BytecodeInterpreter_LMS_Opt
    it.initialize()
    val b = new Bar
    b.intField = 7
    val f = it.compile { (x:Int) => 
        if (x > 0) {
          b.intField = 9
        }
        b.intField
    }
    val y = f(7)
    println(y)
    assert(y == 9)
  }

  def testB2 = withOutFileChecked(prefix+"B2") {
    val it = new BytecodeInterpreter_LMS_Opt
    it.initialize()
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
    val y = f(7)
    println(y)
    assert(y == 14)
  }

  def testB3 = withOutFileChecked(prefix+"B3") {
    val it = new BytecodeInterpreter_LMS_Opt
    it.initialize()
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
    val y = f(7)
    println(y)
    assert(y == 9)
  } 


/* test that needs multiple speculative iterations:

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





  // indirection through object field
/*
  def testC1 = withOutFileChecked(prefix+"C1") {
    val it = new BytecodeInterpreter_Opt
    it.initialize()
    val b = new Bar
    b.intField = 7
    val f = it.compile { (x:Int) => 
        if (x > 0) {
          b.intField = 9
        }
        b.intField
    }
    val y = f(7)
    println(y)
    assert(y == 9)
  }

  def testC2 = withOutFileChecked(prefix+"C2") {
    val it = new BytecodeInterpreter_Opt
    it.initialize()
    val b = new Bar
    b.intField = 7
    val f = it.compile { (x:Int) => 
        while (x > 0) {
          b.intField += 1
          x -= 1
        }
        b.intField
    }
    val y = f(7)
    println(y)
    assert(y == 9)
  }

  def testC3 = withOutFileChecked(prefix+"C3") {
    val it = new BytecodeInterpreter_Opt
    it.initialize()
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
    val y = f(7)
    println(y)
    assert(y == 9)
  } 
*/

}