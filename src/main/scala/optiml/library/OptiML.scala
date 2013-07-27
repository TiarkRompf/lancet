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
package optiml
package library

import java.io.{BufferedReader,FileReader}

import lancet.api._
import lancet.interpreter._
import lancet.core._

/**
 * Library implementation of OptiML language ops
 * 
 * We currently do not handle generic types (we assume Double everywhere).
 * 
 * This whole file is a hacky mess. We should implement it using proper
 * generics (or Forge) if we are doing it for real. The current version
 * uses no generics (either inheritance or type classes) and explicitly 
 * avoids overloading, just to keep things as simple as possible.
 */
 
// object OptiML {
class OptiMLCompanion {
  val DenseVector = new DenseVectorCompanion
  val DenseMatrix = new DenseMatrixCompanion

  /**
   * i/o
   */ 
  def readVector(path: String): DenseVector[Double] = {
    val x = new DenseVector[Double](0, true)
    val xfs = new BufferedReader(new FileReader(path))
    var line = xfs.readLine()    
    while (line != null){      
      line = line.trim()
      x <<= line.toDouble
      line = xfs.readLine()
    }
    xfs.close()    
    x
  }
  
  def readMatrix(path: String): DenseMatrix[Double] = {
    val delim = "\\s+"
    
    val xfs = new BufferedReader(new FileReader(path))
    var line = xfs.readLine()
    line = line.trim()
    var elems = line.split(delim)
    val x = new DenseMatrix[Double](0, elems.length)  
    
    while (line != null){
      val v = index_new(0,elems.length) map { i => elems(i).toDouble } 
      x <<= v  
      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        elems = line.split(delim)
      }
    }
    xfs.close()
    x
  }
  
  /**
   * index vector
   */
   // implicit def intToIndexOp(i: Int): IndexOp = new IndexOp(i)
   // 
   // class IndexOp(val _end: Int) {
   //   def ::(_start: Int): IndexVector = new IndexVectorRange(_start,_end)
   // }  
   def index_new(start: Int, end: Int): IndexVectorRange = new IndexVectorRange(start,end)
   def indexvector_hashreduce(x: IndexVectorRange, f: Int => Int, map: Int => DenseVector[Double], reduce: (DenseVector[Double],DenseVector[Double]) => DenseVector[Double]): DenseVector[DenseVector[Double]] = {
     /* sequential */
     /*
     val out = new DenseVector[DenseVector[Double]](x._end-x._start,true) // every index is a group
     for (i <- x._start until x._end) {
       val grp = f(i)
       val elem = map(i)
       if (out(grp) == null) out(grp) = elem
       else out(grp) = reduce(out(grp),elem)
     }
     */
     
     /* parallel */     
     val grps = new java.util.concurrent.ConcurrentHashMap[Int,DenseVector[Double]]
     for (i <- (x._start until x._end).par) {
       val key = f(i)
       val e = grps.putIfAbsent(key, map(i))
       if (e != null) {
         var newElem = grps.get(key)
         while (!grps.replace(key, newElem, reduce(newElem, map(i)))) {
           newElem = grps.get(key)
         }
       }
     }

     val out = new DenseVector[DenseVector[Double]](x._end-x._start,true) // every index is a group
     for (i <- 0 until out.length) {
       out(i) = grps.get(i)
     }
     out
   }
   
   def indexvector_hashreduce2(x: IndexVectorRange, f: Int => Int, map: Int => Int, reduce: (Int,Int) => Int): DenseVector[Int] = {
     /* sequential */
     /*
     val out = new DenseVector[Int](x._end-x._start,true) // every index is a group
     for (i <- x._start until x._end) {
       val grp = f(i)
       val elem = map(i)
       out(grp) = reduce(out(grp),elem)
     }
     */

     /* parallel */     
     val grps = new java.util.concurrent.ConcurrentHashMap[Int,Int]
     for (i <- (x._start until x._end).par) {
       val key = f(i)
       val v = map(i)
       val e = grps.putIfAbsent(key, v)
       if ((e == 0 && v != 0) || (e != 0)) {
         var newElem = grps.get(key)
         while (!grps.replace(key, newElem, reduce(newElem, v))) {
           newElem = grps.get(key)
         }
       }
     }

     val out = new DenseVector[Int](x._end-x._start,true) // every index is a group
     for (i <- 0 until out.length) {
       out(i) = grps.get(i)
     }
     out 
   }   
   
   /**
    * untilconverged
    */
  
   def untilconverged[T](x: DenseMatrix[T], tol: Double, maxIter: Int, block: DenseMatrix[T] => DenseMatrix[T]): DenseMatrix[T] = {
     var iter = 0
     var last = x
     var converged = false
     while (iter < maxIter && !converged) {
       val next = block(last)
       if (dist3(next,last).asInstanceOf[Double] < tol) {  // hack
         converged = true
       }
       last = next
       iter += 1
     }
     println("Finished in " + iter + " iterations")
     last
   }
   
   def untilconverged2[T](x: DenseVector[T], tol: Double, maxIter: Int, block: DenseVector[T] => DenseVector[T]): DenseVector[T] = {
     var iter = 0
     var last = x
     var converged = false
     while (iter < maxIter && !converged) {
       val next = block(last)
       if (dist2(next,last).asInstanceOf[Double] < tol) {  // hack
         converged = true
       }
       last = next
       iter += 1
     }
     println("Finished in " + iter + " iterations")
     last     
   }
   
   /**
    * sum
    */
  def sum(start: Int, end: Int, size: Int, block: Int => DenseVector[Double]) = {
    val acc = new DenseVector[Double](size, true)
    for (i <- start until end) {
      acc += block(i)
    }
    acc
  }
      
  /**
   * dist
   */
   def dist[T](a: DenseVectorView[T], b: DenseVectorView[T]): T = {
     // hack: assume T is double
     // use sum of squares
     var ans = 0.0
     val aDbl = a.asInstanceOf[DenseVectorView[Double]]
     val bDbl = b.asInstanceOf[DenseVectorView[Double]]
     for (i <- 0 until a._length) {
       ans += (aDbl.apply4(i)-bDbl.apply4(i))*(aDbl.apply4(i)-bDbl.apply4(i))
     }
     ans.asInstanceOf[T]
   }

   def dist2[T](a: DenseVector[T], b: DenseVector[T]): T = {
     // hack: assume T is double
     // use sum of squares
     var ans = 0.0
     val aDbl = a.asInstanceOf[DenseVector[Double]]
     val bDbl = b.asInstanceOf[DenseVector[Double]]
     for (i <- 0 until a.length) {
       ans += (aDbl(i)-bDbl(i))*(aDbl(i)-bDbl(i))
     }
     ans.asInstanceOf[T]
   }
   
   def dist3[T](a: DenseMatrix[T], b: DenseMatrix[T]): T = {
     // hack: assume T is double
     // use sum of squares
     var ans = 0.0
     val aDbl = a.asInstanceOf[DenseMatrix[Double]]
     val bDbl = b.asInstanceOf[DenseMatrix[Double]]
     for (i <- 0 until a.numRows) {
       for (j <- 0 until a.numCols) {
         ans += (aDbl(i,j)-bDbl(i,j))*(aDbl(i,j)-bDbl(i,j))
       }       
     }
     ans.asInstanceOf[T]
   }   
 
 /**
  * math
  */
 def exp(x: Double): Double = Math.exp(x)

 /**
  * profiling
  */
  var profileTime: Long = _
  def tic[T](arg1: T, arg2: T): Unit = {
    profileTime = System.currentTimeMillis()
  }
  def toc[T](arg: T): Unit = {
    val now = System.currentTimeMillis()
    println("(elapsed time: " + (now-profileTime)/1000D + "s)")
  }
}
