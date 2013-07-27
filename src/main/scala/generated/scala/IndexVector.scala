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
package generated.scala



/* IndexVector is an IntVector whose elements represent indices (e.g., of another vector).
 * It is either backed by a discrete sequence of integers (e.g. 1,5,10) or a continouous RangeVector.
 *
 * IndexVectors can be used for scatter/gather operations.
 *
 * They also provide a vector construction operator { } that takes a function mapping an index to a value,
 * producing a new vector.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Dec 27, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class IndexVectorDenseC(__length: Int, __isRow: Boolean) { 
  var _length = __length
  var _isRow = __isRow
  var _data: Array[Int] = new Array[Int](_length)
  
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[Int], len: Int) {
    _data = xs
    _length = len
  }
  
  def Clone = { 
    val v = new IntDenseVector(_length, _isRow);
    v._data = _data.clone
    v
  }    
}

class IndexVectorRange(__start: Int, __end: Int) { 
  var _start = __start
  var _end = __end
  var _stride = 1
  var _isRow = true
  
  def unsafeSetData(xs: Array[Int], len: Int) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }
    
  def Clone = { 
    val len = _end - _start
    val v = new IntDenseVector(len, _isRow)
    var i = 0
    while (i < len) {
      v._data(i) = _start + i
      i += 1
    }
    v
  }  
}
