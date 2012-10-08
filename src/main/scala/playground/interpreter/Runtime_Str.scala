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



trait RuntimeUniverse_Str extends Base_Str with RuntimeUniverse {

object unsafe {

  def monitorEnter(value: Rep[Object]): Rep[Unit] = 
    reflect("unsafe.monitorEnter("+value+")")
  def monitorExit(value: Rep[Object]): Rep[Unit] = 
    reflect("unsafe.monitorExit("+value+")")

  def getObject(base: Rep[Object], offset: Rep[Long]): Rep[Object] = 
    reflect("unsafe.getObject("+base+","+offset+")")
  def getObjectVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Object] = 
    reflect("unsafe.getObjectVolatile("+base+","+offset+")")

  def getBoolean(base: Rep[Object], offset: Rep[Long]): Rep[Boolean] = 
    reflect("unsafe.getBoolean("+base+","+offset+")")
  def getBooleanVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Boolean] = 
    reflect("unsafe.getBooleanVolatile("+base+","+offset+")")

  def getByte(base: Rep[Object], offset: Rep[Long]): Rep[Byte] = 
    reflect("unsafe.getByte("+base+","+offset+")")
  def getByteVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Byte] = 
    reflect("unsafe.getByteVolatile("+base+","+offset+")")

  def getChar(base: Rep[Object], offset: Rep[Long]): Rep[Char] = 
    reflect("unsafe.getChar("+base+","+offset+")")
  def getCharVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Char] = 
    reflect("unsafe.getCharVolatile("+base+","+offset+")")

  def getShort(base: Rep[Object], offset: Rep[Long]): Rep[Short] = 
    reflect("unsafe.getShort("+base+","+offset+")")
  def getShortVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Short] = 
    reflect("unsafe.getShortVolatile("+base+","+offset+")")

  def getInt(base: Rep[Object], offset: Rep[Long]): Rep[Int] = 
    reflect("unsafe.getInt("+base+","+offset+")")
  def getIntVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Int] = 
    reflect("unsafe.getIntVolatile("+base+","+offset+")")

  def getLong(base: Rep[Object], offset: Rep[Long]): Rep[Long] = 
    reflect("unsafe.getLong("+base+","+offset+")")
  def getLongVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Long] = 
    reflect("unsafe.getLongVolatile("+base+","+offset+")")

  def getFloat(base: Rep[Object], offset: Rep[Long]): Rep[Float] = 
    reflect("unsafe.getFloat("+base+","+offset+")")
  def getFloatVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Float] = 
    reflect("unsafe.getFloatVolatile("+base+","+offset+")")

  def getDouble(base: Rep[Object], offset: Rep[Long]): Rep[Double] = 
    reflect("unsafe.getDouble("+base+","+offset+")")
  def getDoubleVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Double] = 
    reflect("unsafe.getDoubleVolatile("+base+","+offset+")")


  def putObject(base: Rep[Object], offset: Rep[Long], value: Rep[Object]): Rep[Unit] = 
    reflect("unsafe.putObject("+base+","+offset+", "+value+")")
  def putObjectVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Object]): Rep[Unit] = 
    reflect("unsafe.putObjectVolatile("+base+","+offset+", "+value+")")

  def putBoolean(base: Rep[Object], offset: Rep[Long], value: Rep[Boolean]): Rep[Unit] = 
    reflect("unsafe.putBoolean("+base+","+offset+", "+value+")")
  def putBooleanVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Boolean]): Rep[Unit] = 
    reflect("unsafe.putBooleanVolatile("+base+","+offset+", "+value+")")

  def putByte(base: Rep[Object], offset: Rep[Long], value: Rep[Byte]): Rep[Unit] = 
    reflect("unsafe.putByte("+base+","+offset+", "+value+")")
  def putByteVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Byte]): Rep[Unit] = 
    reflect("unsafe.putByteVolatile("+base+","+offset+", "+value+")")

  def putChar(base: Rep[Object], offset: Rep[Long], value: Rep[Char]): Rep[Unit] = 
    reflect("unsafe.putChar("+base+","+offset+", "+value+")")
  def putCharVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Char]): Rep[Unit] = 
    reflect("unsafe.putCharVolatile("+base+","+offset+", "+value+")")

  def putShort(base: Rep[Object], offset: Rep[Long], value: Rep[Short]): Rep[Unit] = 
    reflect("unsafe.putShort("+base+","+offset+", "+value+")")
  def putShortVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Short]): Rep[Unit] = 
    reflect("unsafe.putShortVolatile("+base+","+offset+", "+value+")")

  def putInt(base: Rep[Object], offset: Rep[Long], value: Rep[Int]): Rep[Unit] = 
    reflect("unsafe.putInt("+base+","+offset+", "+value+")")
  def putIntVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Int]): Rep[Unit] = 
    reflect("unsafe.putIntVolatile("+base+","+offset+", "+value+")")

  def putLong(base: Rep[Object], offset: Rep[Long], value: Rep[Long]): Rep[Unit] = 
    reflect("unsafe.putLong("+base+","+offset+", "+value+")")
  def putLongVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Long]): Rep[Unit] = 
    reflect("unsafe.putLongVolatile("+base+","+offset+", "+value+")")

  def putFloat(base: Rep[Object], offset: Rep[Long], value: Rep[Float]): Rep[Unit] = 
    reflect("unsafe.putFloat("+base+","+offset+", "+value+")")
  def putFloatVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Float]): Rep[Unit] = 
    reflect("unsafe.putFloatVolatile("+base+","+offset+", "+value+")")

  def putDouble(base: Rep[Object], offset: Rep[Long], value: Rep[Double]): Rep[Unit] = 
    reflect("unsafe.putDouble("+base+","+offset+", "+value+")")
  def putDoubleVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Double]): Rep[Unit] = 
    reflect("unsafe.putDoubleVolatile("+base+","+offset+", "+value+")")




  def allocateInstance(clazz: Class[_]): Rep[Object] = 
    reflect("unsafe.allocateInstance("+clazz+")")

}





class Runtime_Str(metaProvider: MetaAccessProvider) extends Runtime {

    def invoke(method: ResolvedJavaMethod, args: Array[Rep[Object]]): Rep[Object] =
        reflect(""+method+".invoke("+args.mkString(",")+")")

    def typeIsInstance(typ: ResolvedJavaType, value: Rep[Object]): Rep[Boolean] = {
        reflect(""+value+".isInstanceOf["+typ.toJava+"]")
    }

    def monitorEnter(value: Rep[Object]): Unit = {
        nullCheck(value)
        unsafe.monitorEnter(value)
    }

    def monitorExit(value: Rep[Object]): Unit = {
        nullCheck(value)
        unsafe.monitorEnter(value)
    }

    def newObject(typ: ResolvedJavaType): Rep[Object] = { //} throws InstantiationException {
        unsafe.allocateInstance(typ.toJava());
    }

    def newArray(typ: ResolvedJavaType, size: Rep[Int]): Rep[Object] = { // throws InstantiationException {
        reflect("new Array["+typ.toJava()+"]("+size+")");
    }

    def newArray(typ: Class[_], size: Rep[Int]): Rep[Object] = { // throws InstantiationException {
        reflect("new Array["+typ+"]("+size+")");
    }

    def newMultiArray(typ: ResolvedJavaType, dimensions: Array[Rep[Int]]): Rep[Object] = { // throws InstantiationException {
        reflect("new Array["+typ.toJava()+"]("+dimensions.mkString(",")+")");
    }

    def getFieldObject(base: Rep[Object], field: ResolvedJavaField): Rep[AnyRef] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getObjectVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getObject(resolveBase(base, field), offset)
        }
    }

    def getFieldBoolean(base: Rep[Object], field: ResolvedJavaField): Rep[Boolean] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getBooleanVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getBoolean(resolveBase(base, field), offset)
        }
    }

    def getFieldByte(base: Rep[Object], field: ResolvedJavaField): Rep[Byte] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getByteVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getByte(resolveBase(base, field), offset)
        }
    }

    def getFieldChar(base: Rep[Object], field: ResolvedJavaField): Rep[Char] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getCharVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getChar(resolveBase(base, field), offset)
        }
    }

    def getFieldShort(base: Rep[Object], field: ResolvedJavaField): Rep[Short] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getShortVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getShort(resolveBase(base, field), offset)
        }
    }

    def getFieldInt(base: Rep[Object], field: ResolvedJavaField): Rep[Int] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getIntVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getInt(resolveBase(base, field), offset)
        }
    }

    def getFieldLong(base: Rep[Object], field: ResolvedJavaField): Rep[Long] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getLongVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getLong(resolveBase(base, field), offset)
        }
    }

    def getFieldDouble(base: Rep[Object], field: ResolvedJavaField): Rep[Double] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getDoubleVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getDouble(resolveBase(base, field), offset)
        }
    }

    def getFieldFloat(base: Rep[Object], field: ResolvedJavaField): Rep[Float] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getFloatVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getFloat(resolveBase(base, field), offset)
        }
    }

    def setFieldObject(value: Rep[Object], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putObjectVolatile(resolveBase(base, field), offset, value)
        } else {
            unsafe.putObject(resolveBase(base, field), offset, value)
        }
    }

    def setFieldInt(value: Rep[Int], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putIntVolatile(resolveBase(base, field), offset, value)
        } else {
            unsafe.putInt(resolveBase(base, field), offset, value)
        }
    }


    def setFieldFloat(value: Rep[Float], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putFloatVolatile(resolveBase(base, field), offset, value)
        } else {
            unsafe.putFloat(resolveBase(base, field), offset, value)
        }
    }

    def setFieldDouble(value: Rep[Double], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putDouble(resolveBase(base, field), offset, value);
        }
    }

    def setFieldLong(value: Rep[Long], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putDouble(resolveBase(base, field), offset, value);
        }
    }

    def getArrayByte(index: Rep[Long], array: Rep[Object]): Rep[Byte] = {
        checkArray(array, index);
        return unsafe.getByte(array, (Unsafe.ARRAY_BYTE_BASE_OFFSET) + Unsafe.ARRAY_BYTE_INDEX_SCALE.toLong * index);
    }

    def getArrayChar(index: Rep[Long], array: Rep[Object]): Rep[Char] = {
        checkArray(array, index);
        return unsafe.getChar(array, Unsafe.ARRAY_CHAR_BASE_OFFSET + Unsafe.ARRAY_CHAR_INDEX_SCALE * index);
    }

    def getArrayShort(index: Rep[Long], array: Rep[Object]): Rep[Short] = {
        checkArray(array, index);
        return unsafe.getShort(array, Unsafe.ARRAY_SHORT_BASE_OFFSET + Unsafe.ARRAY_SHORT_INDEX_SCALE * index);
    }

    def getArrayInt(index: Rep[Long], array: Rep[Object]): Rep[Int] = {
        checkArray(array, index);
        return unsafe.getInt(array, Unsafe.ARRAY_INT_BASE_OFFSET + Unsafe.ARRAY_INT_INDEX_SCALE * index);
    }

    def getArrayLong(index: Rep[Long], array: Rep[Object]): Rep[Long] = {
        checkArray(array, index);
        return unsafe.getLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index);
    }

    def getArrayDouble(index: Rep[Long], array: Rep[Object]): Rep[Double] = {
        checkArray(array, index);
        return unsafe.getDouble(array, Unsafe.ARRAY_DOUBLE_BASE_OFFSET + Unsafe.ARRAY_DOUBLE_INDEX_SCALE * index);
    }

    def getArrayFloat(index: Rep[Long], array: Rep[Object]): Rep[Float] = {
        checkArray(array, index);
        return unsafe.getFloat(array, Unsafe.ARRAY_FLOAT_BASE_OFFSET + Unsafe.ARRAY_FLOAT_INDEX_SCALE * index);
    }

    def getArrayObject(index: Rep[Long], array: Rep[Object]): Rep[Object] = {
        checkArray(array, index);
        return unsafe.getObject(array, Unsafe.ARRAY_OBJECT_BASE_OFFSET + Unsafe.ARRAY_OBJECT_INDEX_SCALE * index);
    }

    def setArrayByte(value: Rep[Byte], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        if (array.isInstanceOf[Array[Boolean]]) {
            checkArrayType(array, classOf[Boolean]);
        } else {
            checkArrayType(array, classOf[Byte]);
        }
        unsafe.putByte(array, Unsafe.ARRAY_BYTE_BASE_OFFSET + Unsafe.ARRAY_BYTE_INDEX_SCALE * index, value);
    }

    def setArrayChar(value: Rep[Char], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Char]);
        unsafe.putChar(array, Unsafe.ARRAY_CHAR_BASE_OFFSET + Unsafe.ARRAY_CHAR_INDEX_SCALE * index, value);
    }

    def setArrayShort(value: Rep[Short], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Short]);
        unsafe.putShort(array, Unsafe.ARRAY_SHORT_BASE_OFFSET + Unsafe.ARRAY_SHORT_INDEX_SCALE * index, value);
    }

    def setArrayInt(value: Rep[Int], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Int]);
        unsafe.putInt(array, Unsafe.ARRAY_INT_BASE_OFFSET + Unsafe.ARRAY_INT_INDEX_SCALE * index, value);
    }

    def setArrayLong(value: Rep[Long], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Long]);
        unsafe.putLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index, value);
    }

    def setArrayFloat(value: Rep[Float], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Float]);
        unsafe.putFloat(array, Unsafe.ARRAY_FLOAT_BASE_OFFSET + Unsafe.ARRAY_FLOAT_INDEX_SCALE * index, value);
    }

    def setArrayDouble(value: Rep[Double], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Double]);
        unsafe.putDouble(array, Unsafe.ARRAY_DOUBLE_BASE_OFFSET + Unsafe.ARRAY_DOUBLE_INDEX_SCALE * index, value);
    }

    def setArrayObject(value: Rep[Object], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, if (value != null) value.getClass() else null);
        unsafe.putObject(array, Unsafe.ARRAY_OBJECT_BASE_OFFSET + Unsafe.ARRAY_OBJECT_INDEX_SCALE * index, value);
    }

    def nullCheck(value: Rep[Object]): Rep[Object] = 
      reflect("if ("+value+" == null) throw new NullPointerException() else "+value)

    def checkArrayType(array: Rep[Object], arrayType: Class[_]): Unit = reflect("""{
        if (arrayType == null) {
            return;
        }
        val typ: ResolvedJavaType = metaProvider.getResolvedJavaType(array.getClass()).componentType();
        if (!typ.toJava().isAssignableFrom(arrayType)) {
            throw new ArrayStoreException(arrayType.getName());
        }
    }""")

    def checkArray(array: Rep[Object], index: Rep[Long]): Unit = reflect("""{
        nullCheck(array);
        val typ: ResolvedJavaType = metaProvider.getResolvedJavaType(array.getClass());
        if (!typ.isArrayClass()) {
            throw new ArrayStoreException(array.getClass().getName());
        }
        if (index < 0 || index >= arrayLength(array)) {
            throw new ArrayIndexOutOfBoundsException(index.toInt);
        }
    }""")

    def arrayLength(array: Rep[Object]): Rep[Int] = {
        assert(array != null);
        return java.lang.reflect.Array.getLength(array);
    }

    def isVolatile(field: ResolvedJavaField): Boolean = {
        return Modifier.isVolatile(field.accessFlags());
    }

    def resolveOffset(field: ResolvedJavaField): Long = {
        return field.asInstanceOf[HotSpotResolvedJavaField].offset();
    }

    def resolveBase(base: Rep[Object], field: ResolvedJavaField): Rep[Object] = 
      reflect("if ("+base+" == null) "+field.holder().toJava()+" else "+base)

}


}
