package generated.scala


/**
 * Delite
 */

abstract class DeliteOpMultiLoop[A] {
  def size: Int
  var loopStart: Int
  var loopSize: Int
  def alloc: A
  def processRange(__act: A, start: Int, end: Int): A //init+process
  def init(__act: A, idx: Int): A
  def process(__act: A, idx: Int): Unit
  def combine(__act: A, rhs: A): Unit
  def postCombine(__act: A, rhs: A): Unit
  def postProcInit(__act: A): Unit
  def postProcess(__act: A): Unit
  def finalize(__act: A): Unit
}

/**
 * Ref
 */

class Ref[@specialized(Boolean, Int, Long, Float, Double) T](v: T) {
  private[this] var _v = v

  def get = _v
  def set(v: T) = _v = v
}
