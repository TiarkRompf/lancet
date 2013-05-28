package generated.scala

class FloatDenseMatrix(nRows: Int, nCols: Int) { 
  var _numRows = nRows
  var _numCols = nCols
  var _data: Array[Float] = new Array[Float](nRows*nCols)
    
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */
  def unsafeSetData(xs: Array[Float], len: Int) {
    _data = xs
    // _length = len
  }
  
  def Clone = {    
    val res = new FloatDenseMatrix(_numRows, _numCols)
    res._data = _data.clone
    res
  }  
}

