package generated.scala

class DoubleDenseMatrix(nRows: Int, nCols: Int) { 
  var _numRows = nRows
  var _numCols = nCols
  var _data: Array[Double] = new Array[Double](nRows*nCols)
    
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */
  def unsafeSetData(xs: Array[Double], len: Int) {
    _data = xs
    // _length = len
  }
  
  def Clone = {    
    val res = new DoubleDenseMatrix(_numRows, _numCols)
    res._data = _data.clone
    res
  }  
}

