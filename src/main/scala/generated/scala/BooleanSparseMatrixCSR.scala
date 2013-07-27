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

/**
 * A sparse matrix in Compressed Sparse Row (CSR) format, efficient for reads and matrix arithmetic operations.
 * SparseMatrixCOO should be used to construct sparse matrices.
 * 
 * Should we use CSC? At the very least, we should probably have an alternative CSC implementation to experiment with.
 * In ML, m >> n, so CSC would require less storage. However, row slicing (to get a single training example) would be less efficient.
 * Since this is OptiLA, we should probably provide both.
 */
class BooleanSparseMatrixCSR(__numRows: Int, __numCols: Int) { 
  var _numRows = __numRows
  var _numCols = __numCols
  // non-zero values, left-to-right then top-to-bottom
  var _data = new Array[Boolean](32)
  // column index of each non-zero value
  var _colIndices = new Array[Int](32)
  // starting location of each row, e.g. (_rowPtr(3) = 5 means that row 3 starts at value index 5)
  var _rowPtr = new Array[Int](_numRows+1)
  var _nnz = 0

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[Boolean], len: Int) = throw new UnsupportedOperationException("unsafeSetData in SparseMatrixCSR not supported")  
  
  def Clone = { 
    val m = new BooleanSparseMatrixCSR(_numRows, _numCols)
    m._data = _data.clone
    m._colIndices = _colIndices.clone
    m._rowPtr = _rowPtr.clone
    m._nnz = _nnz
    m
  }  
}
