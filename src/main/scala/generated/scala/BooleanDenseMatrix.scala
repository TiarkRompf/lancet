package generated.scala

class BooleanDenseMatrix(nRows: Int, nCols: Int) { 
  var _numRows = nRows
  var _numCols = nCols
  var _data: Array[Boolean] = new Array[Boolean](nRows*nCols)
    
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */
  def unsafeSetData(xs: Array[Boolean], len: Int) {
    _data = xs
    // _length = len
  }
  
  def Clone = {    
    val res = new BooleanDenseMatrix(_numRows, _numCols)
    res._data = _data.clone
    res
  }  
}

