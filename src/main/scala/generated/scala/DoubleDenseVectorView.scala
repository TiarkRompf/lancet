package generated.scala

class DoubleDenseVectorView(val _data: Array[Double], val _start: Int, val _stride: Int, val _length: Int, val _isRow: Boolean) { 

  private def idx(n: Int) = _start + n*_stride

  def apply(n: Int) : Double = {
    _data(idx(n))
  }

  def update(n: Int, x: Double) {
    _data(idx(n)) = x
  }

  def unsafeSetData(xs: Array[Double], len: Int) = throw new UnsupportedOperationException()
}
