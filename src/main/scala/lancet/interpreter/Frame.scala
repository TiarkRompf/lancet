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

package lancet.interpreter

import lancet.core._

import java.lang.reflect.{Array=>jlrArray,_};
import java.util._;
import sun.misc._;

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;


trait InterpreterUniverse extends RuntimeUniverse { univ =>

trait Frame {
  def getObject(index: Int): Rep[Object]
  def setObject(index: Int, value: Rep[Object]): Unit
  def getFloat(index: Int): Rep[Float]
  def setFloat(index: Int, value: Rep[Float]): Unit
  def getLong(index: Int): Rep[Long]
  def setLong(index: Int, value: Rep[Long]): Unit
  def getInt(index: Int): Rep[Int]
  def setInt(index: Int, value: Rep[Int]): Unit
  def getDouble(index: Int): Rep[Double]
  def setDouble(index: Int, value: Rep[Double]): Unit
  def getParentFrame(level: Int): Frame // requires static link
  def setParentFrame(level: Int, frame: Frame): Unit
  def getTopFrame(): Frame
  def getArguments(argOffset: Int): Rep[Array[Object]]
  def size: Int
}

trait InterpreterFrame extends Frame {
  def create(method: ResolvedJavaMethod, hasReceiver: Boolean, additionalStackSpace: Int, useParentArguments: Boolean): InterpreterFrame
  def copy: InterpreterFrame
  def resolveLocalIndex(index: Int): Int
  def depth(): Int
  def stackTos(): Int
  def getReturnValue(): Rep[Object]
  def setReturnValueObject(value: Rep[Object]): Unit
  def setReturnValueInt(value: Rep[Int]): Unit
  def setReturnValueLong(value: Rep[Long]): Unit
  def setReturnValueFloat(value: Rep[Float]): Unit
  def setReturnValueDouble(value: Rep[Double]): Unit
  def peekReceiver(method: ResolvedJavaMethod): Rep[Object]
  //def pushBoth(oValue: Rep[Object], intValue: Rep[Int]): Unit
  //def pushBoth(oValue: Rep[Object], longValue: Rep[Long]): Unit
  def pushObject(value: Rep[Object]): Unit
  def pushBoolean(value: Rep[Boolean]): Unit
  def pushByte(value: Rep[Byte]): Unit
  def pushShort(value: Rep[Short]): Unit
  def pushChar(value: Rep[Char]): Unit
  def pushInt(value: Rep[Int]): Unit
  def pushDouble(value: Rep[Double]): Unit
  def pushFloat(value: Rep[Float]): Unit
  def pushLong(value: Rep[Long]): Unit
  def popBoolean(): Rep[Boolean]
  def popByte(): Rep[Byte]
  def popChar(): Rep[Char]
  def popShort(): Rep[Short]
  def popInt(): Rep[Int]
  def popDouble(): Rep[Double]
  def popFloat(): Rep[Float]
  def popLong(): Rep[Long]
  def popObject(): Rep[Object]
  def swapSingle(): Unit
  def dupx1(): Unit
  def dup2x1(): Unit
  def dup2x2(): Unit
  def dupx2(): Unit
  def dup(length: Int): Unit
  def tosSingle(offset: Int): Int
  def getStackTop(): Int
  def pushVoid(count: Int): Unit
  def popVoid(count: Int): Unit
  def getConstantPool(): ConstantPool
  def setMethod(method: ResolvedJavaMethod): Unit
  def getMethod(): ResolvedJavaMethod
  def setBCI(bci: Int): Unit
  def getBCI(): Int
  def setNextBCI(bci: Int): Unit
  def getNextBCI(): Int
  def getParentFrame(): InterpreterFrame
  def setParentFrame(f: InterpreterFrame): Unit
  def dispose(): Unit
  def popStack(): Unit
}

}
