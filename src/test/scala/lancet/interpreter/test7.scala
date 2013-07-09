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

class TestInterpreter7 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-7"


  class Matrix[A: Manifest](val rows: Int, val cols: Int) {
    private val arr: Array[A] = new Array[A](rows * cols)

    def apply(i: Int, j: Int): A = {
      arr.asInstanceOf[Array[Double]].apply(i*cols + j).asInstanceOf[A]
    }

    def update(i: Int, j: Int, e: A) {
      arr.asInstanceOf[Array[Double]].update(i*cols + j, e.asInstanceOf[Double])
    }
    
    def print {
      println(arr(0))
    }
  }
  
  
  /*def genericTest(rows: Int, cols: Int): () => Unit = { () =>
    val m = randomMatrix(rows, cols)
    val n = randomMatrix(cols, rows)

    val p = multGeneric(m,n)
    p.print
  }

  def specificTest(rows: Int, cols: Int): () => Unit = { () =>
    val m = randomMatrix(rows, cols)
    val n = randomMatrix(cols, rows)

    val p = multDouble(m,n)
    p.print
  }*/

  def randomMatrix(n: Int, m: Int) = {
    val r = new util.Random(10)
    val x = new Matrix[Double](n, m) 
    for (i <- 0 until n; j <- 0 until m)
      x(i, j) = (r.nextInt % 1000).toDouble
    x
  }

  def multGeneric[T:Numeric:Manifest](m: Matrix[T], n: Matrix[T]) = {
    val num = implicitly[Numeric[T]]
    import num._

    val p = new Matrix[T](m.rows, n.cols)
    
    for (i <- 0 until m.rows) {
      for (j <- 0 until n.cols) {
        for (k <- 0 until n.rows)
          p(i, j) += m(i, k) * n(k, j)
      }
    }
    p
  }

/*
  def multDouble(p:Matrix[Double], m: Matrix[Double], n: Matrix[Double]) = {
    //val p = new Matrix[Double](m.rows, n.cols)

    for (i <- 0 until m.rows) {
      for (j <- 0 until n.cols) {
        for (k <- 0 until n.rows)
          p(i, j) += m(i, k) * n(k, j)
      }
    }
    p
  }
*/

  def multDouble(p:Matrix[Double], m: Matrix[Double], n: Matrix[Double]) = {
    //val p = new Matrix[Double](m.rows, n.cols)
    var i = 0
    while (i <= m.rows) {
      var j = 0
      while (j <= n.cols) {
        var k = 0
        while (k <= n.rows) {
          p(i, j) += m(i, k) * n(k, j)
          k+=1
        }
        j+=1
      }
      i+=1
    }
    p
  }



  def test1 = withOutFileChecked(prefix+"mmult1") {
    val it = new Compiler

    val rows = 500
    val cols = 200

    var m1 = randomMatrix(500,200)
    var m2 = randomMatrix(200,500)
    var m3 = randomMatrix(500,500)

    val f = it.compile { (x:Int) => 
      val m = multDouble(m3,m1,m2)
      7
    }
    


    f(100)
  }



  class Compiler extends BytecodeInterpreter_TIR_Opt with DefaultMacros {

    initialize()
    emitUniqueOpt = true
    emitRecursive = true
    debugBlockKeys = false
    debugReadWrite = false
    debugMethods = false
    debugReturns = false

  }

}
