package generated.scala

class IntDenseMatrix(nRows: Int, nCols: Int) { 
  var _numRows = nRows
  var _numCols = nCols
  var _data: Array[Int] = new Array[Int](nRows*nCols)
    
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */
  def unsafeSetData(xs: Array[Int], len: Int) {
    _data = xs
    // _length = len
  }
  
  def Clone = {    
    val res = new IntDenseMatrix(_numRows, _numCols)
    res._data = _data.clone
    res
  }  
}

