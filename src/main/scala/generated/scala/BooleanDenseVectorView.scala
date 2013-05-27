package generated.scala

class BooleanDenseVectorView(val _data: Array[Boolean], val _start: Int, val _stride: Int, val _length: Int, val _isRow: Boolean) { 

  private def idx(n: Int) = _start + n*_stride

  def apply(n: Int) : Boolean = {
    _data(idx(n))
  }

  def update(n: Int, x: Boolean) {
    _data(idx(n)) = x
  }

  def unsafeSetData(xs: Array[Boolean], len: Int) = throw new UnsupportedOperationException()
}
