package generated.scala

class FloatDenseVectorView(val _data: Array[Float], val _start: Int, val _stride: Int, val _length: Int, val _isRow: Boolean) { 

  private def idx(n: Int) = _start + n*_stride

  def apply(n: Int) : Float = {
    _data(idx(n))
  }

  def update(n: Int, x: Float) {
    _data(idx(n)) = x
  }

  def unsafeSetData(xs: Array[Float], len: Int) = throw new UnsupportedOperationException()
}
