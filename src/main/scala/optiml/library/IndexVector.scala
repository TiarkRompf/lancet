package optiml
package library

import lancet.api._
import lancet.interpreter._
import lancet.core._

class IndexVectorRange(val _start: Int, val _end: Int) {
  /* Vector constructor */
  def construct[T](f: Int => T): DenseVector[T] = ??? //map(f)
  
  /* Matrix constructor */
  def constructRows[T](f: Int => DenseVector[T]): DenseMatrix[T] = ???     
  
  def apply(n: Int): Int = ??? //_start + n
  
  /*
  def map[A:Manifest](f: Int => A): DenseVector[A] = {
    ???
    /*
    assert(_start == 0)
    val out = new DenseVector[A](_end-_start,true)
    for (i <- 0 until _end) {
      out(i) = f(i)
    }
    out    
    */
  }
  */
}
