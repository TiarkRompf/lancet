package optiml
package library

import lancet.api._
import lancet.interpreter._
import lancet.core._

class IndexVectorRange(val _start: Int, val _end: Int) {
  /* Vector constructor */
  def construct[T](f: Int => T): DenseVector[T] = {
    assert(_start == 0)
    val out = (new DenseVector[Int](_end-_start,true)).asInstanceOf[DenseVector[T]]
    for (i <- 0 until _end) {
      out(i) = f(i)
    }
    out        
  }
  
  /* Matrix constructor */
  def constructRows[T](f: Int => DenseVector[T]): DenseMatrix[T] = {
    val out = (new DenseMatrix[Double](0,0)).asInstanceOf[DenseMatrix[T]]
    for (i <- _start until _end) {
      out <<= f(i)
    }
    out
  }    
  
  def apply(n: Int): Int = _start + n
  
  def map[T](f: Int => T): DenseVector[T] = {
    assert(_start == 0)
    val out = (new DenseVector[Double](_end-_start,true)).asInstanceOf[DenseVector[T]]
    for (i <- 0 until _end) {
      out(i) = f(i)
    }
    out    
  }
}
