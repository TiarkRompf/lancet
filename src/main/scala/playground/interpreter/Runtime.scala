/*
 * Copyright (c) 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package playground.interpreter


import java.lang.reflect.{Array=>jlrArray,_};
import java.util._;
import sun.misc._;

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;


class HasUnsafe {
    final val unsafe: Unsafe = loadUnsafe();
    private def loadUnsafe(): Unsafe = {
        try {
            return Unsafe.getUnsafe();
        } catch {
            case e: SecurityException =>
        }
        try {
            val theUnsafeInstance: Field = classOf[Unsafe].getDeclaredField("theUnsafe");
            theUnsafeInstance.setAccessible(true);
            return theUnsafeInstance.get(classOf[Unsafe]).asInstanceOf[Unsafe];
        } catch {
            case e: Exception =>
            throw new RuntimeException("exception while trying to get Unsafe.theUnsafe via reflection:", e);
        }
    }
}


trait InterpreterUniverse extends Base {

trait Runtime {
  def invoke(method: ResolvedJavaMethod, args: Array[Rep[Object]]): Rep[Object]
  def typeIsInstance(typ: ResolvedJavaType, value: Rep[Object]): Rep[Boolean]
  def monitorEnter(value: Rep[Object]): Unit
  def monitorExit(value: Rep[Object]): Unit
  def newObject(typ: ResolvedJavaType): Rep[Object] // throws InstantiationException {
  def newArray(typ: ResolvedJavaType, size: Rep[Int]): Rep[Object] // throws InstantiationException {
  // TODO: should have primitive array constructors
  def newArray(typ: Class[_], size: Rep[Int]): Rep[Object] // throws InstantiationException {
  def newMultiArray(typ: ResolvedJavaType, dimensions: Array[Rep[Int]]): Rep[Object] // throws InstantiationException {
  def getFieldObject(base: Rep[Object], field: ResolvedJavaField): Rep[Object]
  def getFieldBoolean(base: Rep[Object], field: ResolvedJavaField): Rep[Boolean]
  def getFieldByte(base: Rep[Object], field: ResolvedJavaField): Rep[Byte]
  def getFieldChar(base: Rep[Object], field: ResolvedJavaField): Rep[Char]
  def getFieldShort(base: Rep[Object], field: ResolvedJavaField): Rep[Short]
  def getFieldInt(base: Rep[Object], field: ResolvedJavaField): Rep[Int]
  def getFieldLong(base: Rep[Object], field: ResolvedJavaField): Rep[Long]
  def getFieldDouble(base: Rep[Object], field: ResolvedJavaField): Rep[Double]
  def getFieldFloat(base: Rep[Object], field: ResolvedJavaField): Rep[Float]
  def setFieldObject(value: Rep[Object], base: Rep[Object], field: ResolvedJavaField): Unit
  def setFieldInt(value: Rep[Int], base: Rep[Object], field: ResolvedJavaField): Unit
  def setFieldFloat(value: Rep[Float], base: Rep[Object], field: ResolvedJavaField): Unit
  def setFieldDouble(value: Rep[Double], base: Rep[Object], field: ResolvedJavaField): Unit
  def setFieldLong(value: Rep[Long], base: Rep[Object], field: ResolvedJavaField): Unit
  def getArrayByte(index: Rep[Long], array: Rep[Object]): Rep[Byte]
  def getArrayChar(index: Rep[Long], array: Rep[Object]): Rep[Char]
  def getArrayShort(index: Rep[Long], array: Rep[Object]): Rep[Short]
  def getArrayInt(index: Rep[Long], array: Rep[Object]): Rep[Int]
  def getArrayLong(index: Rep[Long], array: Rep[Object]): Rep[Long]
  def getArrayDouble(index: Rep[Long], array: Rep[Object]): Rep[Double]
  def getArrayFloat(index: Rep[Long], array: Rep[Object]): Rep[Float]
  def getArrayObject(index: Rep[Long], array: Rep[Object]): Rep[Object]
  def setArrayByte(value: Rep[Byte], index: Rep[Long], array: Rep[Object]): Unit
  def setArrayChar(value: Rep[Char], index: Rep[Long], array: Rep[Object]): Unit
  def setArrayShort(value: Rep[Short], index: Rep[Long], array: Rep[Object]): Unit
  def setArrayInt(value: Rep[Int], index: Rep[Long], array: Rep[Object]): Unit
  def setArrayLong(value: Rep[Long], index: Rep[Long], array: Rep[Object]): Unit
  def setArrayFloat(value: Rep[Float], index: Rep[Long], array: Rep[Object]): Unit
  def setArrayDouble(value: Rep[Double], index: Rep[Long], array: Rep[Object]): Unit
  def setArrayObject(value: Rep[Object], index: Rep[Long], array: Rep[Object]): Unit
  def nullCheck(value: Rep[Object]): Rep[Object]
  def checkArrayType(array: Rep[Object], arrayType: Class[_]): Unit
  def checkArray(array: Rep[Object], index: Rep[Long]): Unit
  def arrayLength(array: Rep[Object]): Rep[Int]
  def isVolatile(field: ResolvedJavaField): Boolean
  def resolveOffset(field: ResolvedJavaField): Long
  def resolveBase(base: Rep[Object], field: ResolvedJavaField): Rep[Object] //used?
}




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
  def getParentFrame(level: Int): Frame // will not work! (dynamic)
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
  def getParentFrame(): InterpreterFrame // will not work! (dynamic)
  def dispose(): Unit
  def popStack(): Unit
}

}
