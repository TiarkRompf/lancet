package generated.scala

class IntDenseVectorView(val _data: Array[Int], val _start: Int, val _stride: Int, val _length: Int, val _isRow: Boolean) { 

  private def idx(n: Int) = _start + n*_stride

  def apply(n: Int) : Int = {
    _data(idx(n))
  }

  def update(n: Int, x: Int) {
    _data(idx(n)) = x
  }

  def unsafeSetData(xs: Array[Int], len: Int) = throw new UnsupportedOperationException()
}
