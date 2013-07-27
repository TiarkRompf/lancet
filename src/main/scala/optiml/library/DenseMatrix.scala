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

import Util._

class DenseMatrixCompanion {
  def apply(numRows: Int, numCols: Int) = new DenseMatrix(numRows, numCols)
}

class DenseMatrix[T:Manifest](__numRows: Int, __numCols: Int) {
  private var _numRows = __numRows
  private var _numCols = __numCols
  private var _data = new Array[T](_numRows*_numCols)
  
  def numRows: Int = _numRows
  def numCols: Int = _numCols
  def size: Int = numRows*numCols
  
  def apply(i: Int, j: Int): T = {
    val offset = i*numCols+j
    _data(offset)
  }
  
  def getRow(i: Int): DenseVectorView[T] = new DenseVectorView(_data, i*numCols, 1, numCols, true)
  
  def update(row: Int, col: Int, y: T) {
    val offset = row*numCols+col
    _data(offset) = y
  }
  
  def <<=(y: DenseVector[T]) = insertRow(numRows, y)
  def insertRow(pos: Int, y: DenseVector[T]) {
    val idx = pos*numCols
    if (size == 0) _numCols = y.length
    insertSpace(idx, numCols)
    for (i <- idx until idx+numCols){
      _data(i) = y(i-idx)
    }
    _numRows = _numRows + 1
  }
  
  def mapRowsToVector[B](f: DenseVectorView[T] => B): DenseVector[B] = {
    val out = (new DenseVector[Double](numRows, true)).asInstanceOf[DenseVector[B]]
    for (i <- 0 until numRows) {
      out(i) = f(this.getRow(i))
    }
    out
  }
  
  def pprint: Unit = {
    for (i <- 0 until numRows) {
      print("[ ")
      for (j <- 0 until numCols) {
        print(this(i,j) + " ")
      }
      println("]")      
    }
  }
  
  protected def ensureExtra(extra: Int) {
    if (_data.length - size < extra) {
      realloc(size+extra)
    }
  }
  
  protected def realloc(minLen: Int) {
    var n = Math.max(4, _data.length * 2)
    while (n < minLen) n = n*2
    val d = new Array[T](n)
    System.arraycopy(_data, 0, d, 0, size)
    _data = d
  }
  
  protected def insertSpace(pos: Int, len: Int) {
    if (pos < 0 || pos > size) throw new IndexOutOfBoundsException()
    ensureExtra(len)
    System.arraycopy(_data, pos, _data, pos+len, size - pos)
  }  
}
