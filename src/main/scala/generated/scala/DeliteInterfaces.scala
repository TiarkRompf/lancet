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
  def init(__act: A, idx: Int, isEmpty:Boolean): A
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
