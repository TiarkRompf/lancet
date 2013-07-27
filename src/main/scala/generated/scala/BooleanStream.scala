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



class BooleanStream(val numRows: Int, val numCols: Int, val chunkSize: Int, val func: (Int,Int) => Boolean, val isPure: Boolean) { //extends BooleanStream {
    val bufRows = math.min(numRows, chunkSize)
    val size = numCols*bufRows
    protected var _data: Array[Boolean] =  try { new Array[Boolean](size) }
                                     catch {
                                       case _ => throw new RuntimeException("Stream overflowed during initialization (numCols = " + numCols + ", chunkSize = " + bufRows + ")")
                                     }
    def data = _data

    def initRow(row: Int, offset: Int) {
      var j = 0
      val lhsOff = row*numCols
      val rhsOff = offset*chunkSize+row
      while (j < numCols) {
        _data(lhsOff+j) = func(rhsOff,j)
        j += 1
      }
    }

    // chunk management must be done inside the op (foreachRows), not in the impl
    // here, we just have to assume that init has been called appropriately and idx points to the right chunk

    def chunkRow(idx: Int, offset: Int) = {
      //vview(idx*numCols, 1, numCols, true)
      new BooleanStreamRow(idx, offset, this, _data)
    }

    def rawElem(idx: Int): Boolean = {
      _data(idx)
    }

    def vview(start: Int, stride: Int, length: Int, isRow: Boolean) = {
      new BooleanDenseVectorView(_data, start, stride, length, isRow)
    }
}
