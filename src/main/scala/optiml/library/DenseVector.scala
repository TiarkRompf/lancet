package optiml
package library

import lancet.api._
import lancet.interpreter._
import lancet.core._

import Util._

class DenseVectorCompanion {
  def rand(n:Int): DenseVector[Double] = { printxx("DenseVector$.rand"); ??? }
  // def apply[T](xs: T*): DenseVector[T] = { printxx("DenseVector$.apply"); ??? }
}

class DenseVectorView[T] {
  def Clone: DenseVector[T] = ???
}

class DenseVector[T/*:Manifest*/](__length: Int, __isRow: Boolean) {
  // private var _length = __length
  // private var _isRow = __isRow
  // private var _data = new Array[T](_length)
  
  def isRow: Boolean = ??? //_isRow
  def length: Int = ??? //_length
      
  def apply(n: Int): T = ??? //_data(n)
  def update(n: Int, y: T) { 
    ???
    // _data(n) = y 
  } 
  
  def t: DenseVector[T] = ???  
  def minIndex: Int = ???
  // def sum: T = ???
  
  def /(b: T): DenseVector[T] = ???
  def +(b: DenseVector[T]): DenseVector[T] = ??? 
}
