package generated.scala

class LongDenseVectorView(val _data: Array[Long], val _start: Int, val _stride: Int, val _length: Int, val _isRow: Boolean) { 

  private def idx(n: Int) = _start + n*_stride

  def apply(n: Int) : Long = {
    _data(idx(n))
  }

  def update(n: Int, x: Long) {
    _data(idx(n)) = x
  }

  def unsafeSetData(xs: Array[Long], len: Int) = throw new UnsupportedOperationException()
}
