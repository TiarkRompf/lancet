/*
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/agpl.html.
 * 
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package optiml
package library

import lancet.api._
import lancet.interpreter._
import lancet.core._

import Util._

class DenseVectorCompanion {
  def rand(n:Int): DenseVector[Double] = { printxx("DenseVector$.rand"); ??? }
  def zeros(n:Int,isRow:Boolean): DenseVector[Double] = { new DenseVector(n,isRow) }
  // def apply[T](xs: T*): DenseVector[T] = { printxx("DenseVector$.apply"); ??? }
}

class DenseVectorView[T:Manifest](val _data: Array[T], val _start: Int, val _stride: Int, val _length: Int, val _isRow: Boolean) { 
  private def idx(n: Int) = _start + n*_stride

  def apply4(n: Int) : T = {
    _data(idx(n))
  }

  def update(n: Int, x: T) {
    _data(idx(n)) = x
  }
  
  def times2(b: T): DenseVector[T] = {
    // hack, assume Double
    val out = new DenseVector(_length, _isRow)
    val thisDbl = this.asInstanceOf[DenseVectorView[Double]]
    val outDbl = out.asInstanceOf[DenseVector[Double]]
    val bDbl = b.asInstanceOf[Double]
    for (i <- 0 until _length) {
      //out(i) = (this.apply4(i).asInstanceOf[Double] * b.asInstanceOf[Double]).asInstanceOf[T]
      outDbl(i) = thisDbl.apply4(i) * bDbl 
    }
    out
  }  
  
  def Clone2: DenseVector[T] = {
    val out = new DenseVector(_length, _isRow)
    for (i <- 0 until _length) {
      out(i) = _data(idx(i))
    }
    out
  }
}

class DenseVector[T:Manifest](__length: Int, __isRow: Boolean) {
  private var _length = __length
  private var _isRow = __isRow
  private var _data = new Array[T](_length)
  
  def isRow: Boolean = _isRow
  def length: Int = _length
      
  def apply(n: Int): T = _data(n)
  def apply2(n: Int): T = _data(n)
  def apply3(n: Int): T = _data(n)
  def update(n: Int, y: T) { 
    _data(n) = y 
  } 
  
  def t: DenseVector[T] = {
    val out = new DenseVector(length, !isRow)
    for (i <- 0 until length) {
      out(i) = this(i)
    }
    out
  }
  
  def pprint: Unit = {
    if (isRow) {
      print("[ ")    
      for (i <- 0 until length) {
        print(this(i) + " ")
      }
      println("]")
    }
    else {
      for (i <- 0 until length) {
        print("[ " + this(i) + " ]")
      }      
    }
  }
  
  def minIndex: Int = {
    // hack: assume T is Double    
    var min = Double.MaxValue
    var minIndex = -1
    val thisDbl = this.asInstanceOf[DenseVector[Double]]
    for (i <- 0 until length) {
      if (thisDbl(i) < min) {
        min = thisDbl(i)
        minIndex = i
      }
    }
    minIndex
  }

  def sum: T = {
    // hack, assume DenseVector[Double]

    // sequential
    //val acc = new DenseVector[Double](this(0).asInstanceOf[DenseVector[Double]].length, true)
    //for (i <- 0 until length) {
    //  acc += this(i).asInstanceOf[DenseVector[Double]]
    //}
    //acc.asInstanceOf[T]

    // parallel
    val acc = _data.par.reduce((a,b) => (a.asInstanceOf[DenseVector[Double]]+b.asInstanceOf[DenseVector[Double]]).asInstanceOf[T])
    acc
  }
  
  def Clone: DenseVector[T] = {
    val out = new DenseVector(_length, _isRow)
    for (i <- 0 until _length) {
      out(i) = _data(i)
    }
    out
  }
  
  def *:*(y: DenseVectorView[T]) = {
    // hack, assumeDouble
    val thisDbl = this.asInstanceOf[DenseVector[Double]]
    val yDbl = y.asInstanceOf[DenseVectorView[Double]]
    var acc = 0.0
    for (i <- 0 until length) {
      acc += thisDbl(i)*yDbl.apply4(i)
    }
    acc.asInstanceOf[T]
  }
  
  def /(b: T): DenseVector[T] = {
    // hack, assume Double
    val thisDbl = this.asInstanceOf[DenseVector[Double]]
    val bDbl = b.asInstanceOf[Double]
    val out = new DenseVector(length, isRow)
    val outDbl = out.asInstanceOf[DenseVector[Double]]
    for (i <- 0 until length) {
      outDbl(i) = (thisDbl(i) / bDbl)
    }
    out
  }
  def -(b: T): DenseVector[T] = {
    // hack, assume Double
    val thisDbl = this.asInstanceOf[DenseVector[Double]]
    val bDbl = b.asInstanceOf[Double]
    val out = new DenseVector(length, isRow)
    val outDbl = out.asInstanceOf[DenseVector[Double]]
    for (i <- 0 until length) {
      outDbl(i) = (thisDbl(i) - bDbl)
    }
    out
  }  
  def +(b: DenseVector[T]): DenseVector[T] = {
    // hack, assume Double
    val thisDbl = this.asInstanceOf[DenseVector[Double]]
    val bDbl = b.asInstanceOf[DenseVector[Double]]
    val out = new DenseVector(length, isRow)
    val outDbl = out.asInstanceOf[DenseVector[Double]]
    for (i <- 0 until length) {
      outDbl(i) = (thisDbl(i) + bDbl(i))
    }
    out    
  } 
  def *(b: T): DenseVector[T] = {
    // hack, assume Double
    val thisDbl = this.asInstanceOf[DenseVector[Double]]
    val bDbl = b.asInstanceOf[Double]
    val out = new DenseVector(length, isRow)
    val outDbl = out.asInstanceOf[DenseVector[Double]]
    for (i <- 0 until length) {
      outDbl(i) = (thisDbl(i) * bDbl)
    }
    out
  }    
  def +=(b: DenseVector[T]): Unit = {
    // hack, assume Double
    val thisDbl = this.asInstanceOf[DenseVector[Double]]
    val bDbl = b.asInstanceOf[DenseVector[Double]]
    for (i <- 0 until length) {
      thisDbl(i) = (thisDbl(i) + bDbl(i))
    }
  }   
  
  def <<=(x: T) = insert(length,x)
  def insert(pos: Int, x: T) {
    insertSpace(pos,1)
    this(pos) = x
  }
  
  protected def insertSpace(pos: Int, len: Int) {
    ensureExtra(len)
    System.arraycopy(_data, pos, _data, pos+len, length-pos)
    _length += len
  }

  protected def ensureExtra(extra: Int) {
    if (_data.length - length < extra) {
      realloc(length + extra)
    }
  }

  protected def realloc(minLen: Int) {  
    var n = Math.max(4, _data.length * 2)
    while (n < minLen) n = n*2
    val d = new Array[T](n)
    System.arraycopy(_data, 0, d, 0, length)
    _data = d
  }
}
