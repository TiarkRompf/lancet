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


trait Base {
  type Rep[T]

  /*implicit def unitWiden[T <% Boolean](x: T): Rep[Boolean]
  implicit def unitWiden[T <% Byte](x: T): Rep[Byte]
  implicit def unitWiden[T <% Char](x: T): Rep[Char]
  implicit def unitWiden[T <% Short](x: T): Rep[Short]
  implicit def unitWiden[T <% Int](x: T): Rep[Int]
  implicit def unitWiden[T <% Long](x: T): Rep[Long]
  implicit def unitWiden[T <% Float](x: T): Rep[Float]
  implicit def unitWiden[T <% Double](x: T): Rep[Double]*/

  implicit def unit(x: Boolean): Rep[Boolean]
  implicit def unit(x: Byte): Rep[Byte]
  implicit def unit(x: Char): Rep[Char]
  implicit def unit(x: Short): Rep[Short]
  implicit def unit(x: Int): Rep[Int]
  implicit def unit(x: Long): Rep[Long]
  implicit def unit(x: Float): Rep[Float]
  implicit def unit(x: Double): Rep[Double]


  implicit def unit(x: Null): Rep[Object]
  def unit(x: Object): Rep[Object]


  implicit def widen[T <% Boolean](x: Rep[T]): Rep[Boolean]
  implicit def widen[T <% Byte](x: Rep[T]): Rep[Byte]
  implicit def widen[T <% Char](x: Rep[T]): Rep[Char]
  implicit def widen[T <% Int](x: Rep[T]): Rep[Int]
  //implicit def widen[T <% Short](x: Rep[T]): Rep[Short]
  implicit def widen[T <% Long](x: Rep[T]): Rep[Long]
  implicit def widen[T <% Float](x: Rep[T]): Rep[Float]
  implicit def widen[T <% Double](x: Rep[T]): Rep[Double]


  implicit def intOps(x: Rep[Int]) = new IntOps(x)
  class IntOps(x: Rep[Int]) {
    def toByte: Rep[Byte] = intToByte(x)
    def toChar: Rep[Char] = intToChar(x)
    def toShort: Rep[Short] = intToShort(x)
    def toInt: Rep[Int] = intToInt(x)
    def toLong: Rep[Long] = intToLong(x)
    def toFloat: Rep[Float] = intToFloat(x)
    def toDouble: Rep[Double] = intToDouble(x)
    def unary_- : Rep[Int] = intNegate(x)
    def +(y: Rep[Int]): Rep[Int] = intPlus(x,y)
    def -(y: Rep[Int]): Rep[Int] = intMinus(x,y)
    def *(y: Rep[Int]): Rep[Int] = intTimes(x,y)
    def /(y: Rep[Int]): Rep[Int] = intDiv(x,y)
    def %(y: Rep[Int]): Rep[Int] = intMod(x,y)
    def &(y: Rep[Int]): Rep[Int] = intAnd(x,y)
    def |(y: Rep[Int]): Rep[Int] = intOr(x,y)
    def ^(y: Rep[Int]): Rep[Int] = intXor(x,y)
    def <<(y: Rep[Int]): Rep[Int] = intShiftLeft(x,y)
    def >>(y: Rep[Int]): Rep[Int] = intShiftRight(x,y)
    def >>>(y: Rep[Int]): Rep[Int] = intShiftRightUnsigned(x,y)
    def <(y: Rep[Int]): Rep[Boolean] = intLess(x,y)
    def <=(y: Rep[Int]): Rep[Boolean] = intLessEqual(x,y)
    def >(y: Rep[Int]): Rep[Boolean] = intGreater(x,y)
    def >=(y: Rep[Int]): Rep[Boolean] = intGreaterEqual(x,y)
    def ===(y: Rep[Int]): Rep[Boolean] = intEqual(x,y)
    def !==(y: Rep[Int]): Rep[Boolean] = intNotEqual(x,y)
  }

  def intToByte(x: Rep[Int]): Rep[Byte]
  def intToChar(x: Rep[Int]): Rep[Char]
  def intToShort(x: Rep[Int]): Rep[Short]
  def intToInt(x: Rep[Int]): Rep[Int]
  def intToLong(x: Rep[Int]): Rep[Long]
  def intToFloat(x: Rep[Int]): Rep[Float]
  def intToDouble(x: Rep[Int]): Rep[Double]

  def intNegate(x: Rep[Int]): Rep[Int]
  def intPlus(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intMinus(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intTimes(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intDiv(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intMod(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intAnd(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intOr(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intXor(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intShiftLeft(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intShiftRight(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intShiftRightUnsigned(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intLess(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intLessEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intGreater(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intGreaterEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intNotEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean]



  implicit def longOps(x: Rep[Long]) = new LongOps(x)
  class LongOps(x: Rep[Long]) {
    def toByte: Rep[Byte] = longToByte(x)
    def toChar: Rep[Char] = longToChar(x)
    def toShort: Rep[Short] = longToShort(x)
    def toInt: Rep[Int] = longToInt(x)
    def toLong: Rep[Long] = longToLong(x)
    def toFloat: Rep[Float] = longToFloat(x)
    def toDouble: Rep[Double] = longToDouble(x)
    def unary_- : Rep[Long] = longNegate(x)
    def +(y: Rep[Long]): Rep[Long] = longPlus(x,y)
    def -(y: Rep[Long]): Rep[Long] = longMinus(x,y)
    def *(y: Rep[Long]): Rep[Long] = longTimes(x,y)
    def /(y: Rep[Long]): Rep[Long] = longDiv(x,y)
    def %(y: Rep[Long]): Rep[Long] = longMod(x,y)
    def &(y: Rep[Long]): Rep[Long] = longAnd(x,y)
    def |(y: Rep[Long]): Rep[Long] = longOr(x,y)
    def ^(y: Rep[Long]): Rep[Long] = longXor(x,y)
    def <<(y: Rep[Long]): Rep[Long] = longShiftLeft(x,y)
    def >>(y: Rep[Long]): Rep[Long] = longShiftRight(x,y)
    def >>>(y: Rep[Long]): Rep[Long] = longShiftRightUnsigned(x,y)
    def <(y: Rep[Long]): Rep[Boolean] = longLess(x,y)
    def <=(y: Rep[Long]): Rep[Boolean] = longLessEqual(x,y)
    def >(y: Rep[Long]): Rep[Boolean] = longGreater(x,y)
    def >=(y: Rep[Long]): Rep[Boolean] = longGreaterEqual(x,y)
    def ===(y: Rep[Long]): Rep[Boolean] = longEqual(x,y)
    def !==(y: Rep[Long]): Rep[Boolean] = longNotEqual(x,y)
  }

  def longToByte(x: Rep[Long]): Rep[Byte]
  def longToChar(x: Rep[Long]): Rep[Char]
  def longToShort(x: Rep[Long]): Rep[Short]
  def longToInt(x: Rep[Long]): Rep[Int]
  def longToLong(x: Rep[Long]): Rep[Long]
  def longToFloat(x: Rep[Long]): Rep[Float]
  def longToDouble(x: Rep[Long]): Rep[Double]

  def longNegate(x: Rep[Long]): Rep[Long]
  def longPlus(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longMinus(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longTimes(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longDiv(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longMod(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longAnd(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longOr(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longXor(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longShiftLeft(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longShiftRight(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longShiftRightUnsigned(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longLess(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longLessEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longGreater(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longGreaterEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longNotEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean]

  implicit def floatOps(x: Rep[Float]) = new FloatOps(x)
  class FloatOps(x: Rep[Float]) {
    def toByte: Rep[Byte] = floatToByte(x)
    def toChar: Rep[Char] = floatToChar(x)
    def toShort: Rep[Short] = floatToShort(x)
    def toInt: Rep[Int] = floatToInt(x)
    def toLong: Rep[Long] = floatToLong(x)
    def toFloat: Rep[Float] = floatToFloat(x)
    def toDouble: Rep[Double] = floatToDouble(x)
    def unary_- : Rep[Float] = floatNegate(x)
    def +(y: Rep[Float]): Rep[Float] = floatPlus(x,y)
    def -(y: Rep[Float]): Rep[Float] = floatMinus(x,y)
    def *(y: Rep[Float]): Rep[Float] = floatTimes(x,y)
    def /(y: Rep[Float]): Rep[Float] = floatDiv(x,y)
    def %(y: Rep[Float]): Rep[Float] = floatMod(x,y)
    def <(y: Rep[Float]): Rep[Boolean] = floatLess(x,y)
    def <=(y: Rep[Float]): Rep[Boolean] = floatLessEqual(x,y)
    def >(y: Rep[Float]): Rep[Boolean] = floatGreater(x,y)
    def >=(y: Rep[Float]): Rep[Boolean] = floatGreaterEqual(x,y)
    def ===(y: Rep[Float]): Rep[Boolean] = floatEqual(x,y)
    def !==(y: Rep[Float]): Rep[Boolean] = floatNotEqual(x,y)
}

  def floatToByte(x: Rep[Float]): Rep[Byte]
  def floatToChar(x: Rep[Float]): Rep[Char]
  def floatToShort(x: Rep[Float]): Rep[Short]
  def floatToInt(x: Rep[Float]): Rep[Int]
  def floatToLong(x: Rep[Float]): Rep[Long]
  def floatToFloat(x: Rep[Float]): Rep[Float]
  def floatToDouble(x: Rep[Float]): Rep[Double]

  def floatNegate(x: Rep[Float]): Rep[Float]
  def floatPlus(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatMinus(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatTimes(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatDiv(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatMod(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatLess(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatLessEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatGreater(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatGreaterEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatNotEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean]

  implicit def doubleOps(x: Rep[Double]) = new DoubleOps(x)
  class DoubleOps(x: Rep[Double]) {
    def toByte: Rep[Byte] = doubleToByte(x)
    def toChar: Rep[Char] = doubleToChar(x)
    def toShort: Rep[Short] = doubleToShort(x)
    def toInt: Rep[Int] = doubleToInt(x)
    def toLong: Rep[Long] = doubleToLong(x)
    def toFloat: Rep[Float] = doubleToFloat(x)
    def toDouble: Rep[Double] = doubleToDouble(x)
    def unary_- : Rep[Double] = doubleNegate(x)
    def +(y: Rep[Double]): Rep[Double] = doublePlus(x,y)
    def -(y: Rep[Double]): Rep[Double] = doubleMinus(x,y)
    def *(y: Rep[Double]): Rep[Double] = doubleTimes(x,y)
    def /(y: Rep[Double]): Rep[Double] = doubleDiv(x,y)
    def %(y: Rep[Double]): Rep[Double] = doubleMod(x,y)
    def <(y: Rep[Double]): Rep[Boolean] = doubleLess(x,y)
    def <=(y: Rep[Double]): Rep[Boolean] = doubleLessEqual(x,y)
    def >(y: Rep[Double]): Rep[Boolean] = doubleGreater(x,y)
    def >=(y: Rep[Double]): Rep[Boolean] = doubleGreaterEqual(x,y)
    def ===(y: Rep[Double]): Rep[Boolean] = doubleEqual(x,y)
    def !==(y: Rep[Double]): Rep[Boolean] = doubleNotEqual(x,y)
  }

  def doubleToByte(x: Rep[Double]): Rep[Byte]
  def doubleToChar(x: Rep[Double]): Rep[Char]
  def doubleToShort(x: Rep[Double]): Rep[Short]
  def doubleToInt(x: Rep[Double]): Rep[Int]
  def doubleToLong(x: Rep[Double]): Rep[Long]
  def doubleToFloat(x: Rep[Double]): Rep[Float]
  def doubleToDouble(x: Rep[Double]): Rep[Double]

  def doubleNegate(x: Rep[Double]): Rep[Double]
  def doublePlus(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleMinus(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleTimes(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleDiv(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleMod(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleLess(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleLessEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleGreater(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleGreaterEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleNotEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean]


  def if_[T](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T]

}

trait Base_Impl extends Base {
  type Rep[T] = T

  /*def unit[T <% Boolean](x: T): Boolean = x
  def unit[T <% Byte](x: T): Byte = x
  def unit[T <% Char](x: T): Char = x
  def unit[T <% Short](x: T): Short = x
  def unit[T <% Int](x: T): Int = x
  def unit[T <% Long](x: T): Long = x
  def unit[T <% Float](x: T): Float = x
  def unit[T <% Double](x: T): Double = x*/
  
  def unit(x: Null): Object = x

  //def widen[T <% Boolean](x: T): Rep[Boolean] = x
  //def widen[T <% Byte](x: T): Rep[Byte] = x
  //def widen[T <% Char](x: T): Rep[Char] = x
  //def widen[T <% Short](x: T): Rep[Short] = x
  def widen[T <% Int](x: T): Rep[Int] = x
  def widen[T <% Long](x: T): Rep[Long] = x
  def widen[T <% Float](x: T): Rep[Float] = x
  def widen[T <% Double](x: T): Rep[Double] = x

  def intToByte(x: Int): Byte = x.toByte
  def intToChar(x: Int): Char = x.toChar
  def intToShort(x: Int): Short = x.toShort

  def intNegate(x: Int): Int = -x
  def intPlus(x: Int, y: Int): Int = x + y
  def intTimes(x: Int, y: Int): Int = x * y
  def intAnd(x: Int, y: Int): Int = x & y
  def intOr(x: Int, y: Int): Int = x | y
  def intXor(x: Int, y: Int): Int = x ^ y

  def longNegate(x: Long): Long = -x
  def longPlus(x: Long, y: Long): Long = x + y
  def longTimes(x: Long, y: Long): Long = x * y
  def longAnd(x: Long, y: Long): Long = x & y
  def longOr(x: Long, y: Long): Long = x | y
  def longXor(x: Long, y: Long): Long = x ^ y
  
  def floatNegate(x: Float): Float = -x
  def floatPlus(x: Float, y: Float): Float = x + y
  def floatTimes(x: Float, y: Float): Float = x * y

  def doubleNegate(x: Double): Double = -x
  def doublePlus(x: Double, y: Double): Double = x + y
  def doubleTimes(x: Double, y: Double): Double = x * y
}



trait InterpreterUniverse extends Base {

trait Runtime {
  def invoke(method: ResolvedJavaMethod, args: Array[Rep[Object]]): Rep[Object]
  def monitorEnter(value: Rep[Object]): Unit
  def monitorExit(value: Rep[Object]): Unit
  def newObject(typ: ResolvedJavaType): Rep[Object] // throws InstantiationException {
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
  def getParentFrame(level: Int): Frame
  def getTopFrame(): Frame
  def getArguments(argOffset: Int): Array[Rep[Object]]
  def size: Int
}

trait InterpreterFrame extends Frame {
  def create(method: ResolvedJavaMethod, hasReceiver: Boolean, additionalStackSpace: Int, useParentArguments: Boolean): InterpreterFrame
  def resolveLocalIndex(index: Int): Int
  def depth(): Int
  def stackTos(): Int
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
  def getParentFrame(): InterpreterFrame
  def dispose(): Unit
  def popStack(): Unit
}

}



trait InterpreterUniverse_Impl extends Base_Impl with InterpreterUniverse {

object Runtime extends HasUnsafe

class Runtime_Impl(metaProvider: MetaAccessProvider) extends Runtime {

    //val delegate = Graal.getRuntime().getCapability(classOf[RuntimeInterpreterInterface]);

    import Runtime._

    val toJava = classOf[HotSpotResolvedJavaMethod].getDeclaredMethod("toJava")
    toJava.setAccessible(true)

    def invoke(method: ResolvedJavaMethod, args: Array[AnyRef]): AnyRef = {
      val m = toJava.invoke(method).asInstanceOf[java.lang.reflect.Method]

      //println("invoking: " + m + " " + args.mkString("(",",",")") + "//" + args.length)
      //println("types: " + m.getParameterTypes.mkString(",") + "//" + m.getParameterTypes.length)
      m.invoke(null, args:_*) // FIXME:?? what about non-static method ??
    }

    def monitorEnter(value: Object): Unit = {
        nullCheck(value);
        unsafe.monitorEnter(value);
    }

    def monitorExit(value: Object): Unit = {
        nullCheck(value);
        unsafe.monitorExit(value);
    }

    def newObject(typ: ResolvedJavaType): AnyRef = { //} throws InstantiationException {
        return unsafe.allocateInstance(typ.toJava());
    }

    def getFieldObject(base: Object, field: ResolvedJavaField): AnyRef = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            return unsafe.getObjectVolatile(resolveBase(base, field), offset);
        } else {
            return unsafe.getObject(resolveBase(base, field), offset);
        }
    }

    def getFieldBoolean(base: Object, field: ResolvedJavaField): Boolean = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            return unsafe.getBooleanVolatile(resolveBase(base, field), offset);
        } else {
            return unsafe.getBoolean(resolveBase(base, field), offset);
        }
    }

    def getFieldByte(base: Object, field: ResolvedJavaField): Byte = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            return unsafe.getByteVolatile(resolveBase(base, field), offset);
        } else {
            return unsafe.getByte(resolveBase(base, field), offset);
        }
    }

    def getFieldChar(base: Object, field: ResolvedJavaField): Char = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            return unsafe.getCharVolatile(resolveBase(base, field), offset);
        } else {
            return unsafe.getChar(resolveBase(base, field), offset);
        }
    }

    def getFieldShort(base: Object, field: ResolvedJavaField): Short = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            return unsafe.getShortVolatile(resolveBase(base, field), offset);
        } else {
            return unsafe.getShort(resolveBase(base, field), offset);
        }
    }

    def getFieldInt(base: Object, field: ResolvedJavaField): Int = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            return unsafe.getIntVolatile(resolveBase(base, field), offset);
        } else {
            return unsafe.getInt(resolveBase(base, field), offset);
        }
    }

    def getFieldLong(base: Object, field: ResolvedJavaField): Long = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            return unsafe.getLongVolatile(resolveBase(base, field), offset);
        } else {
            return unsafe.getLong(resolveBase(base, field), offset);
        }
    }

    def getFieldDouble(base: Object, field: ResolvedJavaField): Double = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            return unsafe.getDoubleVolatile(resolveBase(base, field), offset);
        } else {
            return unsafe.getDouble(resolveBase(base, field), offset);
        }
    }

    def getFieldFloat(base: Object, field: ResolvedJavaField): Float = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            return unsafe.getFloatVolatile(resolveBase(base, field), offset);
        } else {
            return unsafe.getFloat(resolveBase(base, field), offset);
        }
    }

    def setFieldObject(value: Object, base: Object, field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putObjectVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putObject(resolveBase(base, field), offset, value);
        }
    }

    def setFieldInt(value: Int, base: Object, field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putIntVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putInt(resolveBase(base, field), offset, value);
        }
    }


    def setFieldFloat(value: Float, base: Object, field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putFloatVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putFloat(resolveBase(base, field), offset, value);
        }
    }

    def setFieldDouble(value: Double, base: Object, field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putDouble(resolveBase(base, field), offset, value);
        }
    }

    def setFieldLong(value: Long, base: Object, field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putDouble(resolveBase(base, field), offset, value);
        }
    }

    def getArrayByte(index: Long, array: Object): Byte = {
        checkArray(array, index);
        return unsafe.getByte(array, Unsafe.ARRAY_BYTE_BASE_OFFSET + Unsafe.ARRAY_BYTE_INDEX_SCALE * index);
    }

    def getArrayChar(index: Long, array: Object): Char = {
        checkArray(array, index);
        return unsafe.getChar(array, Unsafe.ARRAY_CHAR_BASE_OFFSET + Unsafe.ARRAY_CHAR_INDEX_SCALE * index);
    }

    def getArrayShort(index: Long, array: Object): Short = {
        checkArray(array, index);
        return unsafe.getShort(array, Unsafe.ARRAY_SHORT_BASE_OFFSET + Unsafe.ARRAY_SHORT_INDEX_SCALE * index);
    }

    def getArrayInt(index: Long, array: Object): Int = {
        checkArray(array, index);
        return unsafe.getInt(array, Unsafe.ARRAY_INT_BASE_OFFSET + Unsafe.ARRAY_INT_INDEX_SCALE * index);
    }

    def getArrayLong(index: Long, array: Object): Long = {
        checkArray(array, index);
        return unsafe.getLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index);
    }

    def getArrayDouble(index: Long, array: Object): Double = {
        checkArray(array, index);
        return unsafe.getDouble(array, Unsafe.ARRAY_DOUBLE_BASE_OFFSET + Unsafe.ARRAY_DOUBLE_INDEX_SCALE * index);
    }

    def getArrayFloat(index: Long, array: Object): Float = {
        checkArray(array, index);
        return unsafe.getFloat(array, Unsafe.ARRAY_FLOAT_BASE_OFFSET + Unsafe.ARRAY_FLOAT_INDEX_SCALE * index);
    }

    def getArrayObject(index: Long, array: Object): Object = {
        checkArray(array, index);
        return unsafe.getObject(array, Unsafe.ARRAY_OBJECT_BASE_OFFSET + Unsafe.ARRAY_OBJECT_INDEX_SCALE * index);
    }

    def setArrayByte(value: Byte, index: Long, array: Object): Unit = {
        checkArray(array, index);
        if (array.isInstanceOf[Array[Boolean]]) {
            checkArrayType(array, classOf[Boolean]);
        } else {
            checkArrayType(array, classOf[Byte]);
        }
        unsafe.putByte(array, Unsafe.ARRAY_BYTE_BASE_OFFSET + Unsafe.ARRAY_BYTE_INDEX_SCALE * index, value);
    }

    def setArrayChar(value: Char, index: Long, array: Object): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Char]);
        unsafe.putChar(array, Unsafe.ARRAY_CHAR_BASE_OFFSET + Unsafe.ARRAY_CHAR_INDEX_SCALE * index, value);
    }

    def setArrayShort(value: Short, index: Long, array: Object): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Short]);
        unsafe.putShort(array, Unsafe.ARRAY_SHORT_BASE_OFFSET + Unsafe.ARRAY_SHORT_INDEX_SCALE * index, value);
    }

    def setArrayInt(value: Int, index: Long, array: Object): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Int]);
        unsafe.putInt(array, Unsafe.ARRAY_INT_BASE_OFFSET + Unsafe.ARRAY_INT_INDEX_SCALE * index, value);
    }

    def setArrayLong(value: Long, index: Long, array: Object): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Long]);
        unsafe.putLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index, value);
    }

    def setArrayFloat(value: Float, index: Long, array: Object): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Float]);
        unsafe.putFloat(array, Unsafe.ARRAY_FLOAT_BASE_OFFSET + Unsafe.ARRAY_FLOAT_INDEX_SCALE * index, value);
    }

    def setArrayDouble(value: Double, index: Long, array: Object): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Double]);
        unsafe.putDouble(array, Unsafe.ARRAY_DOUBLE_BASE_OFFSET + Unsafe.ARRAY_DOUBLE_INDEX_SCALE * index, value);
    }

    def setArrayObject(value: Object, index: Long, array: Object): Unit = {
        checkArray(array, index);
        checkArrayType(array, if (value != null) value.getClass() else null);
        unsafe.putObject(array, Unsafe.ARRAY_OBJECT_BASE_OFFSET + Unsafe.ARRAY_OBJECT_INDEX_SCALE * index, value);
    }

    def nullCheck(value: Object): Object = {
        if (value == null) {
            throw new NullPointerException();
        }
        value
    }

    def checkArrayType(array: Object, arrayType: Class[_]): Unit = {
        if (arrayType == null) {
            return;
        }
        val typ: ResolvedJavaType = metaProvider.getResolvedJavaType(array.getClass()).componentType();
        if (!typ.toJava().isAssignableFrom(arrayType)) {
            throw new ArrayStoreException(arrayType.getName());
        }
    }

    def checkArray(array: Object, index: Long): Unit = {
        nullCheck(array);
        val typ: ResolvedJavaType = metaProvider.getResolvedJavaType(array.getClass());
        if (!typ.isArrayClass()) {
            throw new ArrayStoreException(array.getClass().getName());
        }
        if (index < 0 || index >= arrayLength(array)) {
            throw new ArrayIndexOutOfBoundsException(index.toInt);
        }
    }

    def arrayLength(array: Object): Int = {
        assert(array != null);
        return java.lang.reflect.Array.getLength(array);
    }

    def isVolatile(field: ResolvedJavaField): Boolean = {
        return Modifier.isVolatile(field.accessFlags());
    }

    def resolveOffset(field: ResolvedJavaField): Long = {
        return field.asInstanceOf[HotSpotResolvedJavaField].offset();
    }

    def resolveBase(base: Object, field: ResolvedJavaField): Object = {
        var accessorBase = base;
        if (accessorBase == null) {
            accessorBase = field.holder().toJava();
        }
        return accessorBase;
    }

}



object Frame extends HasUnsafe {
    final val EMPTY_ARRAY = new Array[Object](0)
    final val PARENT_FRAME_SLOT = 0;
    final val MIN_FRAME_SIZE = 1;
}



class Frame_Impl(numLocals: Int, parent: Frame) extends Frame {
    import Frame._
    assert(numLocals >= MIN_FRAME_SIZE);

    val locals: Array[Object] = new Array[Object](numLocals)
    val primitiveLocals: Array[Long] = new Array[Long](numLocals)

    locals(PARENT_FRAME_SLOT) = parent

    def this(numLocals: Int) = this(numLocals, null);

    def getObject(index: Int): Object = {
        return locals(index);
    }

    def setObject(index: Int, value: Object): Unit = {
        locals(index) = value;
    }

    def getFloat(index: Int): Float = {
        return unsafe.getFloat(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setFloat(index: Int, value: Float): Unit = {
        unsafe.putFloat(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getLong(index: Int): Long = {
        return unsafe.getLong(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setLong(index: Int, value: Long): Unit = {
        unsafe.putLong(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getInt(index: Int): Int = {
        return unsafe.getInt(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setInt(index: Int, value: Int): Unit = {
        unsafe.putInt(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getDouble(index: Int): Double = {
        return unsafe.getDouble(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setDouble(index: Int, value: Double): Unit = {
        unsafe.putDouble(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getParentFrame(level: Int): Frame = {
        assert(level >= 0);
        if (level == 0) {
            return this;
        } else {
            return getObject(PARENT_FRAME_SLOT).asInstanceOf[Frame].getParentFrame(level - 1);
        }
    }

    def getTopFrame(): Frame = {
        val parentFrame = getObject(PARENT_FRAME_SLOT).asInstanceOf[Frame];
        if (parentFrame == null) {
            return this;
        } else {
            return parentFrame.getTopFrame();
        }
    }

    def getArguments(argOffset: Int): Array[Object] = {
        return getObject(argOffset).asInstanceOf[Array[Object]];
    }

    def size: Int = {
        return locals.length;
    }
    
}


object InterpreterFrame {
    final val BASE_LENGTH = 3;

    final val METHOD_FRAME_SLOT = 1;
    final val BCI_FRAME_SLOT = 2;

    final val DOUBLE = 2;
    final val SINGLE = 1;
}


class InterpreterFrame_Impl(method: ResolvedJavaMethod, parent: InterpreterFrame, additionalStackSpace: Int) 
extends Frame_Impl(method.maxLocals() + method.maxStackSize() + InterpreterFrame.BASE_LENGTH + additionalStackSpace, parent) 
with InterpreterFrame {

    import Frame._
    import InterpreterFrame._

    assert(additionalStackSpace >= 0);

    setMethod(method);
    setBCI(0);
        

    /** Pointer to the top-most stack frame element. */
    private var tos: Int = BASE_LENGTH;

    def this(method: ResolvedJavaMethod, additionalStackSpace: Int) {
        this(method, null, additionalStackSpace);
    }

    def create(method: ResolvedJavaMethod, hasReceiver: Boolean, additionalStackSpace: Int, useParentArguments: Boolean): InterpreterFrame = {
        val frame = new InterpreterFrame_Impl(method, this, additionalStackSpace);

        if (useParentArguments) {
            val length = method.signature().argumentSlots(hasReceiver);
            assert(length >= 0);

            frame.pushVoid(method.maxLocals());
            if (length > 0) {
                copyArguments(frame, length);
                popVoid(length);
            }
        }

        return frame;
    }

    def resolveLocalIndex(index: Int): Int = {
        assert(index >= 0);
        return BASE_LENGTH + index;
    }

    def depth(): Int = {
        var depth = 1;
        var frame: InterpreterFrame = this;
        while ({ frame = frame.getParentFrame(); frame != null}) {
            depth+=1;
        }
        return depth;
    }

    def stackTos(): Int = {
        return BASE_LENGTH + getMethod().maxLocals();
    }

    private def copyArguments(dest: InterpreterFrame_Impl, length: Int): Unit = {
        System.arraycopy(locals, tosSingle(length - 1), dest.locals,
                        BASE_LENGTH, length);
        System.arraycopy(primitiveLocals, tosSingle(length - 1), dest.primitiveLocals,
                        BASE_LENGTH, length);
    }


    def peekReceiver(method: ResolvedJavaMethod): Object = {
        return getObject(tosSingle(method.signature().argumentSlots(false)));
    }

    def pushBoth(oValue: Object, intValue: Int): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), oValue);
        setInt(tosSingle(0), intValue);
    }

    def pushBoth(oValue: Object, longValue: Long): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), oValue);
        setLong(tosSingle(0), longValue);
    }

    def pushObject(value: Object): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), value);
    }

    def pushBoolean(value: Boolean): Unit = {
        pushInt(if (value) 1 else 0);
    }

    def pushByte(value: Byte): Unit = {
        pushInt(value);
    }

    def pushShort(value: Short): Unit = {
        pushInt(value);
    }

    def pushChar(value: Char): Unit = {
        pushInt(value);
    }

    def pushInt(value: Int): Unit = {
        incrementTos(SINGLE);
        setInt(tosSingle(0), value);
    }

    def pushDouble(value: Double): Unit = {
        incrementTos(DOUBLE);
        setDouble(tosDouble(0), value);
    }

    def pushFloat(value: Float): Unit = {
        incrementTos(SINGLE);
        setFloat(tosSingle(0), value);
    }

    def pushLong(value: Long): Unit = {
        incrementTos(DOUBLE);
        setLong(tosDouble(0), value);
    }

    def popBoolean(): Boolean = {
        val value = popInt();
        assert(value == 0 || value == 1);
        return value == 1;
    }

    def popByte(): Byte = {
        val value = popInt();
        assert (value >= Byte.MinValue && value <= Byte.MaxValue);
        return value.toByte;
    }

    def popChar(): Char = {
        val value = popInt();
        assert (value >= Char.MinValue && value <= Char.MaxValue);
        return value.toChar;
    }

    def popShort(): Short = {
        val value = popInt();
        assert (value >= Short.MinValue && value <= Short.MaxValue);
        return value.toShort;
    }

    def popInt(): Int = {
        val value = getInt(tosSingle(0));
        decrementTos(SINGLE);
        return value;
    }

    def popDouble(): Double = {
        val value = getDouble(tosDouble(0));
        decrementTos(DOUBLE);
        return value;
    }

    def popFloat(): Float = {
        val value = getFloat(tosSingle(0));
        decrementTos(SINGLE);
        return value;
    }

    def popLong(): Long = {
        val value = getLong(tosDouble(0));
        decrementTos(DOUBLE);
        return value;
    }

    def popObject(): Object = {
        val value = getObject(tosSingle(0));
        decrementTos(SINGLE);
        return value;
    }

    def swapSingle(): Unit = {
        val tmpInt = getInt(tosSingle(1));
        val tmpObject = getObject(tosSingle(1));

        setInt(tosSingle(1), getInt(tosSingle(0)));
        setObject(tosSingle(1), getObject(tosSingle(0)));

        setInt(tosSingle(0), tmpInt);
        setObject(tosSingle(0), tmpObject);
    }

    def dupx1(): Unit = {
        val tosLong = getLong(tosSingle(0));
        val tosObject = getObject(tosSingle(0));

        swapSingle();

        pushBoth(tosObject, tosLong);
    }

    def dup2x1(): Unit = {
        val tosLong2 = getLong(tosSingle(2));
        val tosObject2 = getObject(tosSingle(2));
        val tosLong1 = getLong(tosSingle(1));
        val tosObject1 = getObject(tosSingle(1));
        val tosLong0 = getLong(tosSingle(0));
        val tosObject0 = getObject(tosSingle(0));

        popVoid(3);

        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);

        pushBoth(tosObject2, tosLong2);

        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);
    }

    def dup2x2(): Unit = {
        val tosLong3 = getLong(tosSingle(3));
        val tosObject3 = getObject(tosSingle(3));
        val tosLong2 = getLong(tosSingle(2));
        val tosObject2 = getObject(tosSingle(2));
        val tosLong1 = getLong(tosSingle(1));
        val tosObject1 = getObject(tosSingle(1));
        val tosLong0 = getLong(tosSingle(0));
        val tosObject0 = getObject(tosSingle(0));

        popVoid(4);

        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);

        pushBoth(tosObject3, tosLong3);
        pushBoth(tosObject2, tosLong2);

        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);
    }

    def dupx2(): Unit = {
        val tosLong2 = getLong(tosSingle(2));
        val tosObject2 = getObject(tosSingle(2));
        val tosLong1 = getLong(tosSingle(1));
        val tosObject1 = getObject(tosSingle(1));
        val tosLong0 = getLong(tosSingle(0));
        val tosObject0 = getObject(tosSingle(0));

        popVoid(3);

        pushBoth(tosObject0, tosLong0);
        pushBoth(tosObject2, tosLong2);
        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);
    }

    def dup(length: Int): Unit = {
        assert (length > 0);
        var i = 0
        while (i < length) {
            val valueN1 = getLong(tosSingle(length - 1));
            val valueO1 = getObject(tosSingle(length - 1));

            pushVoid(1);

            setLong(tosSingle(0), valueN1);
            setObject(tosSingle(0), valueO1);
            i += 1
        }
    }

    private def incrementTos(size: Int): Unit = {
        assert (size >= 0);
        tos += size;
    }

    private def decrementTos(size: Int): Unit = {
        assert (size >= 0);
        assert (tos - size >= stackTos());
        tos -= size;
    }

    private def tosDouble(offset: Int): Int = {
        assert (offset >= 0);
        return tos - DOUBLE - (offset * DOUBLE);
    }

    def tosSingle(offset: Int): Int = {
        assert (offset >= 0);
        return tos - SINGLE - offset;
    }

    def getStackTop(): Int = {
        return tos;
    }

    def pushVoid(count: Int): Unit = {
        incrementTos(count * SINGLE);
    }

    def popVoid(count: Int): Unit = {
        decrementTos(count * SINGLE);
    }

    def getConstantPool(): ConstantPool = {
        return getMethod().getConstantPool();
    }

    def setMethod(method: ResolvedJavaMethod): Unit = {
        setObject(METHOD_FRAME_SLOT, method);
    }

    def getMethod(): ResolvedJavaMethod = {
        return getObject(METHOD_FRAME_SLOT).asInstanceOf[ResolvedJavaMethod];
    }

    def setBCI(bci: Int): Unit = {
        setInt(BCI_FRAME_SLOT, bci);
    }

    def getBCI(): Int = {
        return getInt(BCI_FRAME_SLOT);
    }

    /*def pushTo(childFrame: InterpreterFrame, argumentSlots: Int): Unit = {
        System.arraycopy(locals, tos - argumentSlots, childFrame.locals,
                        Frame.MIN_FRAME_SIZE, argumentSlots);

        System.arraycopy(primitiveLocals, tos - argumentSlots, childFrame.primitiveLocals,
                        Frame.MIN_FRAME_SIZE, argumentSlots);
        popVoid(argumentSlots);
    }*/

    def getParentFrame(): InterpreterFrame = {
        return getObject(PARENT_FRAME_SLOT).asInstanceOf[InterpreterFrame];
    }

    def dispose(): Unit = {
        // Clear out references in locals array.
        Arrays.fill(locals, null);
    }

    override def toString(): String = {
        val method = getMethod();
        val b = new StringBuilder(getMethod().toStackTraceElement(getBCI()).toString());
        for (i <- 0 until tos) {
            val obj = getObject(tosSingle(i));
            val primitive = getLong(tosSingle(i));

            var objectString: String = null;
            if (obj != null) {
                objectString = obj.getClass().getSimpleName() + "@" + Integer.toHexString(obj.hashCode());
            }
            val primitiveString = "0x" + java.lang.Long.toHexString(primitive).toUpperCase();
            var typeString: String = null;

            val index = tosSingle(i);
            if (index == METHOD_FRAME_SLOT) {
                typeString = "method";
            } else if (index == BCI_FRAME_SLOT) {
                typeString = "bci";
            } else if (index == PARENT_FRAME_SLOT) {
                typeString = "parent";
            } else if (index < BASE_LENGTH + method.maxLocals()) {
                typeString = "local " + (index - BASE_LENGTH);
            } else {
                typeString = "stack";
            }
            b.append(String.format("%n [%d] %7s Primitive: %10s Object: %s", index:Integer, typeString, primitiveString, objectString));
        }
        if (getParentFrame() != null) {
            b.append("\n").append(getParentFrame().toString());
        }
        return b.toString();
    }

    def popStack(): Unit = {
        // TODO(chumer): prevent popping local variables.
        popVoid(tos - stackTos());
    }

}

}
