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

import lancet.api._
import lancet.interpreter._
import lancet.core._

class IndexVectorRange(val _start: Int, val _end: Int) {
  /* Vector constructor */
  def construct[T](f: Int => T): DenseVector[T] = {
    assert(_start == 0)
    val out = (new DenseVector[Int](_end-_start,true)).asInstanceOf[DenseVector[T]]
    for (i <- (0 until _end).par) {
      out(i) = f(i)
    }
    out        
  }

  def construct2[T](f: Int => T): DenseVector[T] = {
    assert(_start == 0)
    val out = (new DenseVector[DenseVector[Double]](_end-_start,true)).asInstanceOf[DenseVector[T]]
    for (i <- (0 until _end).par) {
      out(i) = f(i)
    }
    out        
  }
  
  /* Matrix constructor */
  def constructRows[T](f: Int => DenseVector[T]): DenseMatrix[T] = {
    val out = (new DenseMatrix[Double](0,0)).asInstanceOf[DenseMatrix[T]]
    for (i <- _start until _end) {
      out <<= f(i)
    }
    out
  }    
  
  def apply(n: Int): Int = _start + n
  
  def map[T](f: Int => T): DenseVector[T] = {
    assert(_start == 0)
    val out = (new DenseVector[Double](_end-_start,true)).asInstanceOf[DenseVector[T]]
    for (i <- 0 until _end) {
      out(i) = f(i)
    }
    out    
  }
}
