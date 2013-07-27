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
package generated.scala.container


// specialization bug on multiple ctors: (_indices: Array[Int], _keys: Array[K], _values: Array[V], _sz: Int)
final class HashMapImpl[@specialized K: Manifest](indsz: Int, datasz: Int) {
  private val loadfactor_d2 = 0.4f / 2
  private var indices = Array.fill[Int](HashMapImpl.nextPow2(indsz))(-1)
  private var keys = new Array[K](datasz)
  private var blocksizes: Array[Int] = _
  private var sz = 0
  private var relbits = Integer.numberOfTrailingZeros(indices.length / 2)
  
  import HashMapImpl.nextPow2
  
  // def this(indsz: Int, datasz: Int) = this(
  //   Array.fill[Int](HashMapImpl.nextPow2(indsz))(-1),
  //   new Array[K](datasz),
  //   new Array[V](datasz), 
  //   0)
  def this() = this(128, 52)
  
  @inline private def absolute(hc: Int) = {
    val mask = hc >> 31
    (hc + mask) ^ mask
  }
  
  def size = sz
  
  def get(k: K): Int = {
    val hc = k.## * 0x9e3775cd
    val relbits0 = relbits
    var pos = (hc >>> (32 - relbits0)) * 2
    var currelem = indices(pos)
    var currhash = indices(pos + 1)
    
    while (currelem != -1 && (currhash != hc || keys(currelem) != k)) {
      pos = (pos + 2) & (relbits0+1)
      currelem = indices(pos)
      currhash = indices(pos + 1)
    }
    
    currelem
  }
  
  def put(k: K): Int = {
    val hc = k.## * 0x9e3775cd
    val relbits0 = relbits
    var pos = (hc >>> (32 - relbits0)) * 2
    var currelem = indices(pos)
    var currhash = indices(pos + 1)
    
    while (currelem != -1 && (currhash != hc || keys(currelem) != k)) {
      pos = (pos + 2) & (relbits0+1)
      currelem = indices(pos)
      currhash = indices(pos + 1)
    }
    
    if (currelem == -1) {
      val datapos = sz
      indices(pos) = datapos
      indices(pos + 1) = hc
      keys(datapos) = k
      sz += 1
      
      grow()
      datapos
    } else {
      val datapos = currelem
      keys(datapos) = k
      datapos
    }
  }
  
  def statistics = {
"""size: %d
indices length: %d
data length: %d
growth threshold: %d
""".format(sz, indices.length, keys.length, (loadfactor_d2 * indices.length).toInt)
  }
  
  private def grow() = if (sz > (loadfactor_d2 * indices.length)) {
    val nindices = Array.fill[Int](indices.length * 2)(-1)
    val nkeys = new Array[K](keys.length * 2)
    relbits = Integer.numberOfTrailingZeros(nindices.length / 2)
    
    // copy raw data
    System.arraycopy(keys, 0, nkeys, 0, sz)
    
    // copy indices
    var i = 0
    val relbits0 = relbits
    while (i < indices.length) {
      val elem = indices(i)
      if (elem != -1) {
        val hash = indices(i + 1)
        var pos = (hash >>> (32 - relbits0)) * 2
        
        // insert it into nindices
        var currelem = nindices(pos)
        var currhash = nindices(pos + 1)
        while (currelem != -1) {
          pos = (pos + 2) & (relbits0+1)
          currelem = nindices(pos)
          currhash = nindices(pos + 1)
        }
        nindices(pos) = elem
        nindices(pos + 1) = hash
      }
      i += 2
    }
    
    indices = nindices
    keys = nkeys
  }
  
  override def toString = "HashMapImpl(sz: %d; indices: %s; keys: %s)".format(sz, if (indices != null) indices.mkString(", ") else "null", if (keys != null) keys.mkString(", ") else "null")
  
  def unsafeIndices: Array[Int] = indices
  
  def unsafeKeys: Array[K] = keys
  
  def unsafeSize = sz
  
  def unsafeBlockSizes = blocksizes
  
  def unsafeSetBlockSizes(_blkszs: Array[Int]) = blocksizes = _blkszs
  
  def unsafeSetKeys(_keys: Array[K]) {
    keys = _keys
  }
  
  def unsafeSetSize(_sz: Int) {
    sz = _sz
  }
  
  def unsafeSetInternal(_ind: Array[Int], _keys: Array[K], _sz: Int) {
    indices = _ind
    keys = _keys
    sz = _sz
  }
}


final class Bucket[@specialized T] {
  var array: Array[T] = _
  var size = 0
  //var next: Bucket[T] = _
  
  //def dcSize = size
  def dcApply(idx: Int) = array(idx)
  def dcUpdate(idx: Int, x: T) = array(idx) = x
  
  override def toString = "Bucket(size: %d; values: %s)".format(size, array.take(size).mkString(", "))
}


object HashMapImpl {
  def range(n: Int) = {
    val hm = new HashMapImpl[Int](n * 5 + 1, n * 3)
    for (i <- 0 until n) hm.put(i)
    hm
  }
  def nextPow2(x: Int) = {
    var c = x - 1;
    c |= c >>>  1;
    c |= c >>>  2;
    c |= c >>>  4;
    c |= c >>>  8;
    c |= c >>> 16;
    c + 1;
  }
}
