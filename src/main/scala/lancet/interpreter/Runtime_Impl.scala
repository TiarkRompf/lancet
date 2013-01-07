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



trait RuntimeUniverse_Impl extends Core_Impl with RuntimeUniverse {

object Runtime extends HasUnsafe

class Runtime_Impl(metaProvider: MetaAccessProvider) extends Runtime {

    //val delegate = Graal.getRuntime().getCapability(classOf[RuntimeInterpreterInterface]);

    import Runtime._

    val toJavaM = classOf[HotSpotResolvedJavaMethod].getDeclaredMethod("toJava")
    toJavaM.setAccessible(true)

    def invoke(method: ResolvedJavaMethod, args: Array[AnyRef]): AnyRef = {
      //System.out.println("prepare invoke: " + method + " " + args.mkString("(",",",")") + "//" + args.length)

      def methToJava(m: ResolvedJavaMethod) = toJavaM.invoke(method).asInstanceOf[java.lang.reflect.Method]

      val m = methToJava(method)
      m.setAccessible(true)

      //System.out.println("invoking: " + m + " " + args.mkString("(",",",")") + "//" + args.length)
      //System.out.println("types: " + m.getParameterTypes.mkString(",") + "//" + m.getParameterTypes.length)

      val static = Modifier.isStatic(method.accessFlags)

      if (static)
        m.invoke(null, args:_*)
      else
        m.invoke(args.head, args.tail:_*)
    }

    def typeIsInstance(typ: ResolvedJavaType, value: Object): Boolean = {
      typ.toJava().isInstance(value)
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

    def newArray(typ: ResolvedJavaType, size: Int): Object = { // throws InstantiationException {
        return java.lang.reflect.Array.newInstance(typ.toJava(), size);
    }

    def newArray(typ: Class[_], size: Int): Object = { // throws InstantiationException {
        return java.lang.reflect.Array.newInstance(typ, size);
    }

    def newMultiArray(typ: ResolvedJavaType, dimensions: Array[Int]): Object = { // throws InstantiationException {
        return java.lang.reflect.Array.newInstance(typ.toJava(), dimensions:_*);
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

    def objectGetClass(base: Object): Class[Object] = base.getClass.asInstanceOf[Class[Object]]
    def classGetName(base: Class[Object]): String = base.getName
    def classIsArray(base: Class[Object]): Boolean = base.isArray
    def classGetComponentType(base: Class[Object]): Class[Object] = base.getComponentType.asInstanceOf[Class[Object]]
    def classIsAssignableFrom(base: Class[Object], other: Class[Object]) = base.isAssignableFrom(other)

}

}