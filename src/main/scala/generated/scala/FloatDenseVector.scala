package generated.scala

/**
 * This is the actual class that gets instantiated in the generated code. Ops corresponding to public operations
 * here must have CodeGen methods defined by the DSL on them.
 *
 * Alternatively, everything in this class could be lifted, and we could generate a concrete class to be instantiated
 * in the generated code.
 */

class FloatDenseVector(__length: Int, __isRow: Boolean) { 
  var _length = __length
  var _isRow = __isRow
  var _data: Array[Float] = new Array[Float](_length)

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[Float], len: Int) {
    _data = xs
    _length = len
  }
  
  def Clone = { 
    val v = new FloatDenseVector(_length, _isRow);
    v._data = _data.clone
    v
  }  
}
