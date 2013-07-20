/*
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/agpl.html.
 * 
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

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

class TestInterpreter3LMSD extends BaseTestInterpreter3 {
  val prefix = "test-out/test-interpreter-3-LMSD-"
  def newCompiler = new BytecodeInterpreter_LMS_Opt with Compiler {
    initialize()
    override def defaultHandler = execMethodPostDom
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