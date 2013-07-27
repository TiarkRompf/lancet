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

/*
 * A sparse matrix in COOrdinate format. Efficient for constructing sparse matrices
 * and converting to other (CSR, CSC) formats.
 */ 
class SparseMatrixCOO[@specialized T: Manifest](__numRows: Int, __numCols: Int) { 
  var _numRows = __numRows
  var _numCols = __numCols
  // non-zero values, left-to-right then top-to-bottom
  var _data = new Array[T](32)
  // column index of each non-zero value
  var _colIndices = new Array[Int](32)
  // row index of each non-zero value
  var _rowIndices = new Array[Int](32)
  // number of non-zeros stored in the COO matrix
  var _nnz = 0

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException("unsafeSetData in SparseMatrixCOO not supported")  
  
  def Clone = { 
    val m = new SparseMatrixCOO[T](_numRows, _numCols)
    m._data = _data.clone
    m._colIndices = _colIndices.clone
    m._rowIndices = _rowIndices.clone
    m._nnz = _nnz
    m
  }  
}
