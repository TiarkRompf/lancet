package generated.scala

class LongDenseMatrix(nRows: Int, nCols: Int) { 
  var _numRows = nRows
  var _numCols = nCols
  var _data: Array[Long] = new Array[Long](nRows*nCols)
    
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */
  def unsafeSetData(xs: Array[Long], len: Int) {
    _data = xs
    // _length = len
  }
  
  def Clone = {    
    val res = new LongDenseMatrix(_numRows, _numCols)
    res._data = _data.clone
    res
  }  
}

