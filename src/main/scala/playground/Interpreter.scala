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
package playground

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

object RuntimeIntf extends HasUnsafe

class RuntimeIntf(metaProvider: MetaAccessProvider) {

    //val delegate = Graal.getRuntime().getCapability(classOf[RuntimeInterpreterInterface]);

    import RuntimeIntf._

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

    def nullCheck(value: Object): Unit = {
        if (value == null) {
            throw new NullPointerException();
        }
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



class Frame(numLocals: Int, parent: Frame) {
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


class InterpreterFrame(method: ResolvedJavaMethod, parent: InterpreterFrame, additionalStackSpace: Int) 
extends Frame(method.maxLocals() + method.maxStackSize() + InterpreterFrame.BASE_LENGTH + additionalStackSpace, parent) {

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
        val frame = new InterpreterFrame(method, this, additionalStackSpace);

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
        var frame = this;
        while ({ frame = frame.getParentFrame(); frame != null}) {
            depth+=1;
        }
        return depth;
    }

    def stackTos(): Int = {
        return BASE_LENGTH + getMethod().maxLocals();
    }

    private def copyArguments(dest: InterpreterFrame, length: Int): Unit = {
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

    def pushTo(childFrame: InterpreterFrame, argumentSlots: Int): Unit = {
        System.arraycopy(locals, tos - argumentSlots, childFrame.locals,
                        Frame.MIN_FRAME_SIZE, argumentSlots);

        System.arraycopy(primitiveLocals, tos - argumentSlots, childFrame.primitiveLocals,
                        Frame.MIN_FRAME_SIZE, argumentSlots);
        popVoid(argumentSlots);
    }

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







trait InterpreterCallable {
    // methods
    def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, arguments: Array[Object]): Object // throws Throwable;
}

object InterpreterCallable {
  val INTERPRETER_CALLABLE_INVOKE_NAME = "invoke";
  val INTERPRETER_CALLABLE_INVOKE_SIGNATURE = Array[Class[_]](classOf[InterpreterFrame], classOf[ResolvedJavaMethod], classOf[Array[Object]]);
}


/**
 * Thrown if executed byte code caused an error in {@link BytecodeInterpreter}. The actual execution exception is
 * accessible using {@link #getCause()} or {@link #getExecutionThrowable()}.
 */
class InterpreterException(cause: Throwable) extends RuntimeException(cause) {
    //private static final long serialVersionUID = 1L;
    def getExecutionThrowable() = getCause()
}





// bytecode interpreter

/**
 * High-level bytecode interpreter that executes on top of Java. Java native methods
 * are executed using the {@link com.oracle.graal.api.interpreter.RuntimeInterpreterInterface}.
 */

object BytecodeInterpreter {
    private final val OPTION_MAX_STACK_SIZE = "maxStackSize";
    private final val TRACE = true;
    private final val TRACE_BYTE_CODE = true;

    private final val DEFAULT_MAX_STACK_SIZE = 1500;

    private final val NEXT = -1;
    private final val BRANCH = -2;
    private final val RETURN = -3;
    private final val CALL = -4;
}


//@SuppressWarnings("static-method")
final class BytecodeInterpreter {

    import BytecodeInterpreter._

    private var callFrame: InterpreterFrame = _

    private var methodDelegates: Map[ResolvedJavaMethod, MethodRedirectionInfo] = _;

    private var maxStackFrames: Int = _;

    private var rootMethod: ResolvedJavaMethod = _;
    private var runtimeInterface: RuntimeIntf = _;
    private var metaAccessProvider: MetaAccessProvider = _;

    def initialize(args: String): Boolean = {
        methodDelegates = new HashMap
        maxStackFrames = DEFAULT_MAX_STACK_SIZE;

        val runtime: GraalRuntime = Graal.getRuntime();
        //TR
        //this.runtimeInterface = runtime.getCapability(RuntimeInterpreterInterface.class);
        //if (this.runtimeInterface == null) {
        //    throw new UnsupportedOperationException("The provided graal runtime does not support the required capability " + RuntimeInterpreterInterface.class.getName() + ".");
        //}
        this.metaAccessProvider = runtime.getCapability(classOf[MetaAccessProvider]);
        if (this.metaAccessProvider == null) {
            throw new UnsupportedOperationException("The provided graal runtime does not support the required capability " + classOf[MetaAccessProvider].getName() + ".");
        }
        this.runtimeInterface = new RuntimeIntf(metaAccessProvider)

        this.rootMethod = resolveRootMethod();
        registerDelegates();
        return parseArguments(args);
    }

    def setOption(name: String, value: String): Unit = {
        if (name != null && name.equals(OPTION_MAX_STACK_SIZE)) {
            this.maxStackFrames = Integer.parseInt(value);
        }
    }

    def registerDelegates(): Unit = {
        addDelegate(findMethod(classOf[Throwable], "fillInStackTrace"), new InterpreterCallable() {

            //@Override
            def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, arguments: Array[Object]): Object = {// throws Throwable {
                setBackTrace(caller, arguments(0).asInstanceOf[Throwable], createStackTraceElements(caller));
                return null;
            }
        });
        addDelegate(findMethod(classOf[Throwable], "getStackTraceDepth"), new InterpreterCallable() {

            //@Override
            def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, arguments: Array[Object]): Object = {// throws Throwable {
                val elements: Array[StackTraceElement] = getBackTrace(caller, arguments(0).asInstanceOf[Throwable]);
                if (elements != null) {
                    return elements.length: Integer;
                }
                return 0: Integer;
            }
        });
        addDelegate(findMethod(classOf[Throwable], "getStackTraceElement", classOf[Int]), new InterpreterCallable() {

            //@Override
            def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, arguments: Array[Object]): Object = {//} throws Throwable {
                val elements: Array[StackTraceElement] = getBackTrace(caller, arguments(0).asInstanceOf[Throwable]);
                if (elements != null) {
                    val index: Integer = arguments(0).asInstanceOf[Integer];
                    if (index != null) {
                        return elements(index);
                    }
                }
                return null;
            }
        });
    }

    //@SuppressWarnings("unused")
    def parseArguments(stringArgs: String): Boolean = {
        // TODO: parse the arguments
        return true;
    }

    def setMaxStackFrames(maxStackSize: Int): Unit = {
        this.maxStackFrames = maxStackSize;
    }

    def getMaxStackFrames(): Int = {
        return maxStackFrames;
    }

    def addDelegate(method: Method, callable: InterpreterCallable): Unit = {
        val resolvedMethod: ResolvedJavaMethod = metaAccessProvider.getResolvedJavaMethod(method);
        if (methodDelegates.containsKey(resolvedMethod)) {
            throw new IllegalArgumentException("Delegate for method " + method + " already added.");
        }

        methodDelegates.put(resolvedMethod, new MethodRedirectionInfo(callable));
    }

    def removeDelegate(method: Method): Unit = {
        methodDelegates.remove(metaAccessProvider.getResolvedJavaMethod(method));
    }

    //@Override
    def execute(method: ResolvedJavaMethod, boxedArguments: Array[Object]): Object = {// throws Throwable {
        try {
            val receiver: Boolean = hasReceiver(method);
            val signature: Signature = method.signature();
            assert(boxedArguments != null);
            assert(signature.argumentCount(receiver) == boxedArguments.length);

            if (TRACE) {
                //if (nativeFrame == null) {
                    trace(0, "Executing root method " + method);
                //} else {
                //    trace(nativeFrame.depth(), "Executing from native " + method);
                //}
            }


            var rootFrame: InterpreterFrame = null // nativeFrame
            if (rootFrame == null) {
              rootFrame = new InterpreterFrame(rootMethod, signature.argumentSlots(true));
              rootFrame.pushObject(this);
              rootFrame.pushObject(method);
              rootFrame.pushObject(boxedArguments);
            }
            

            // TODO (chaeubl): invoke the first method in the same way as any other method (the method might be redirected!)
            val firstFrame: InterpreterFrame = rootFrame.create(method, receiver, 0, false);
            initializeLocals(firstFrame, method, boxedArguments);
            executeRoot(rootFrame, firstFrame);

            /*if (TRACE) {
                if (nativeFrame == null) {
                    trace(0, "Returning to root method " + method);
                } else {
                    trace(nativeFrame.depth(), "Returning to native " + method);
                }
            }*/

            return popAsObject(rootFrame, signature.returnKind());
        } catch {
            case e: Exception =>
            // TODO (chaeubl): remove this exception handler (only used for debugging)
            throw e;
        }/* finally {
            nativeCallerFrame.set(nativeFrame);
        }*/
    }

    def initializeLocals(rootFrame: InterpreterFrame, method: ResolvedJavaMethod, boxedArguments: Array[Object]) {
        val receiver: Boolean = hasReceiver(method);
        val signature: Signature = method.signature();
        var index = 0;
        if (receiver) {
            pushAsObject(rootFrame, Kind.Object, boxedArguments(index));
            index += 1;
        }

        var i = 0
        while (index < boxedArguments.length) {
            pushAsObject(rootFrame, signature.argumentKindAt(i), boxedArguments(index));
            i += 1
            index += 1
        }
        // push the remaining locals
        rootFrame.pushVoid(rootFrame.stackTos() - rootFrame.getStackTop());
    }

    def execute(javaMethod: Method, boxedArguments: Array[Object]): Object = {// throws Throwable {
        return execute(metaAccessProvider.getResolvedJavaMethod(javaMethod), boxedArguments);
    }

    def hasReceiver(method: ResolvedJavaMethod): Boolean = {
        return !Modifier.isStatic(method.accessFlags());
    }

    def executeRoot(root: InterpreterFrame, frame: InterpreterFrame): Unit = { // throws Throwable {
        // TODO reflection redirection
        var prevFrame: InterpreterFrame = frame;
        var currentFrame: InterpreterFrame = frame;
        var bs: BytecodeStream = new BytecodeStream(currentFrame.getMethod().code());
        if (TRACE) {
            traceCall(frame, "Call");
        }
        while (currentFrame != root) {
            if (prevFrame != currentFrame) {
                bs = new BytecodeStream(currentFrame.getMethod().code());
            }
            bs.setBCI(currentFrame.getBCI());

            prevFrame = currentFrame;
            currentFrame = loop(root, prevFrame, bs);
        }
        assert(callFrame == null);
    }

    private def loop(root: InterpreterFrame, frame: InterpreterFrame, bs: BytecodeStream): InterpreterFrame = {// throws Throwable {
        try {
            while (true) {
                val result: Int = executeInstruction(frame, bs);
                result match {
                    case NEXT =>
                        bs.next();
                    case RETURN =>
                        return popFrame(frame);
                    case CALL =>
                        return allocateFrame(frame, bs);
                    case BRANCH =>
                        bs.setBCI(bs.readBranchDest());
                    case _ =>
                        // the outcome depends on stack values
                        assert(result >= 0, "negative branch target");
                        bs.setBCI(result);
                }
            }
            null
        } catch {
            case t: Throwable =>
            if (TRACE) {
                traceOp(frame, "Exception " + t.toString());
                t.printStackTrace
            }
            updateStackTrace(frame, t);

            // frame bci needs to be in sync when handling exceptions
            frame.setBCI(bs.currentBCI());

            val handlerFrame: InterpreterFrame = handleThrowable(root, frame, t);
            if (handlerFrame == null) {
                // matched root we just throw it again.
                throw t;
            } else {
                if (TRACE) {
                    traceOp(frame, "Handler found " + handlerFrame.getMethod() + ":" + handlerFrame.getBCI());
                }
                // update bci from frame
                bs.setBCI(handlerFrame.getBCI());

                // continue execution on the found frame
                return handlerFrame;
            }
        } finally {
            // TODO may be not necessary.
            frame.setBCI(bs.currentBCI());
        }
    }

    private def executeInstruction(frame: InterpreterFrame, bs: BytecodeStream): Int = {// throws Throwable {
        if (TRACE_BYTE_CODE) {
            traceOp(frame, bs.currentBCI() + ": " + Bytecodes.baseNameOf(bs.currentBC()));
        }
        (bs.currentBC()) match {
            case Bytecodes.NOP =>
            case Bytecodes.ACONST_NULL=>
                frame.pushObject(null);
            case Bytecodes.ICONST_M1 =>
                frame.pushInt(-1);
            case Bytecodes.ICONST_0 =>
                frame.pushInt(0);
            case Bytecodes.ICONST_1 =>
                frame.pushInt(1);
            case Bytecodes.ICONST_2 =>
                frame.pushInt(2);
            case Bytecodes.ICONST_3 =>
                frame.pushInt(3);
            case Bytecodes.ICONST_4 =>
                frame.pushInt(4);
            case Bytecodes.ICONST_5 =>
                frame.pushInt(5);
            case Bytecodes.LCONST_0 =>
                frame.pushLong(0L);
            case Bytecodes.LCONST_1 =>
                frame.pushLong(1L);
            case Bytecodes.FCONST_0 =>
                frame.pushFloat(0.0F);
            case Bytecodes.FCONST_1 =>
                frame.pushFloat(1.0F);
            case Bytecodes.FCONST_2 =>
                frame.pushFloat(2.0F);
            case Bytecodes.DCONST_0 =>
                frame.pushDouble(0.0D);
            case Bytecodes.DCONST_1 =>
                frame.pushDouble(1.0D);
            case Bytecodes.BIPUSH =>
                frame.pushInt(bs.readByte());
            case Bytecodes.SIPUSH =>
                frame.pushInt(bs.readShort());
            case Bytecodes.LDC | Bytecodes.LDC_W | Bytecodes.LDC2_W =>
                pushCPConstant(frame, bs.readCPI());
            case Bytecodes.ILOAD =>
                frame.pushInt(frame.getInt(frame.resolveLocalIndex(bs.readLocalIndex())));
            case Bytecodes.LLOAD =>
                frame.pushLong(frame.getLong(frame.resolveLocalIndex(bs.readLocalIndex())));
            case Bytecodes.FLOAD =>
                frame.pushFloat(frame.getFloat(frame.resolveLocalIndex(bs.readLocalIndex())));
            case Bytecodes.DLOAD =>
                frame.pushDouble(frame.getDouble(frame.resolveLocalIndex(bs.readLocalIndex())));
            case Bytecodes.ALOAD =>
                frame.pushObject(frame.getObject(frame.resolveLocalIndex(bs.readLocalIndex())));
            case Bytecodes.ILOAD_0 =>
                frame.pushInt(frame.getInt(frame.resolveLocalIndex(0)));
            case Bytecodes.ILOAD_1 =>
                frame.pushInt(frame.getInt(frame.resolveLocalIndex(1)));
            case Bytecodes.ILOAD_2 =>
                frame.pushInt(frame.getInt(frame.resolveLocalIndex(2)));
            case Bytecodes.ILOAD_3 =>
                frame.pushInt(frame.getInt(frame.resolveLocalIndex(3)));
            case Bytecodes.LLOAD_0 =>
                frame.pushLong(frame.getLong(frame.resolveLocalIndex(0)));
            case Bytecodes.LLOAD_1 =>
                frame.pushLong(frame.getLong(frame.resolveLocalIndex(1)));
            case Bytecodes.LLOAD_2 =>
                frame.pushLong(frame.getLong(frame.resolveLocalIndex(2)));
            case Bytecodes.LLOAD_3 =>
                frame.pushLong(frame.getLong(frame.resolveLocalIndex(3)));
            case Bytecodes.FLOAD_0 =>
                frame.pushFloat(frame.getFloat(frame.resolveLocalIndex(0)));
            case Bytecodes.FLOAD_1 =>
                frame.pushFloat(frame.getFloat(frame.resolveLocalIndex(1)));
            case Bytecodes.FLOAD_2 =>
                frame.pushFloat(frame.getFloat(frame.resolveLocalIndex(2)));
            case Bytecodes.FLOAD_3 =>
                frame.pushFloat(frame.getFloat(frame.resolveLocalIndex(3)));
            case Bytecodes.DLOAD_0 =>
                frame.pushDouble(frame.getDouble(frame.resolveLocalIndex(0)));
            case Bytecodes.DLOAD_1 =>
                frame.pushDouble(frame.getDouble(frame.resolveLocalIndex(1)));
            case Bytecodes.DLOAD_2 =>
                frame.pushDouble(frame.getDouble(frame.resolveLocalIndex(2)));
            case Bytecodes.DLOAD_3 =>
                frame.pushDouble(frame.getDouble(frame.resolveLocalIndex(3)));
            case Bytecodes.ALOAD_0 =>
                frame.pushObject(frame.getObject(frame.resolveLocalIndex(0)));
            case Bytecodes.ALOAD_1 =>
                frame.pushObject(frame.getObject(frame.resolveLocalIndex(1)));
            case Bytecodes.ALOAD_2 =>
                frame.pushObject(frame.getObject(frame.resolveLocalIndex(2)));
            case Bytecodes.ALOAD_3 =>
                frame.pushObject(frame.getObject(frame.resolveLocalIndex(3)));
            case Bytecodes.IALOAD =>
                frame.pushInt(runtimeInterface.getArrayInt(frame.popInt(), frame.popObject()));
            case Bytecodes.LALOAD =>
                frame.pushLong(runtimeInterface.getArrayLong(frame.popInt(), frame.popObject()));
            case Bytecodes.FALOAD =>
                frame.pushFloat(runtimeInterface.getArrayFloat(frame.popInt(), frame.popObject()));
            case Bytecodes.DALOAD =>
                frame.pushDouble(runtimeInterface.getArrayDouble(frame.popInt(), frame.popObject()));
            case Bytecodes.AALOAD =>
                frame.pushObject(runtimeInterface.getArrayObject(frame.popInt(), frame.popObject()));
            case Bytecodes.BALOAD =>
                frame.pushInt(runtimeInterface.getArrayByte(frame.popInt(), frame.popObject()));
            case Bytecodes.CALOAD =>
                frame.pushInt(runtimeInterface.getArrayChar(frame.popInt(), frame.popObject()));
            case Bytecodes.SALOAD =>
                frame.pushInt(runtimeInterface.getArrayShort(frame.popInt(), frame.popObject()));
            case Bytecodes.ISTORE =>
                frame.setInt(frame.resolveLocalIndex(bs.readLocalIndex()), frame.popInt());
            case Bytecodes.LSTORE =>
                frame.setLong(frame.resolveLocalIndex(bs.readLocalIndex()), frame.popLong());
            case Bytecodes.FSTORE =>
                frame.setFloat(frame.resolveLocalIndex(bs.readLocalIndex()), frame.popFloat());
            case Bytecodes.DSTORE =>
                frame.setDouble(frame.resolveLocalIndex(bs.readLocalIndex()), frame.popDouble());
            case Bytecodes.ASTORE =>
                frame.setObject(frame.resolveLocalIndex(bs.readLocalIndex()), frame.popObject());
            case Bytecodes.ISTORE_0 =>
                frame.setInt(frame.resolveLocalIndex(0), frame.popInt());
            case Bytecodes.ISTORE_1 =>
                frame.setInt(frame.resolveLocalIndex(1), frame.popInt());
            case Bytecodes.ISTORE_2 =>
                frame.setInt(frame.resolveLocalIndex(2), frame.popInt());
            case Bytecodes.ISTORE_3 =>
                frame.setInt(frame.resolveLocalIndex(3), frame.popInt());
            case Bytecodes.LSTORE_0 =>
                frame.setLong(frame.resolveLocalIndex(0), frame.popLong());
            case Bytecodes.LSTORE_1 =>
                frame.setLong(frame.resolveLocalIndex(1), frame.popLong());
            case Bytecodes.LSTORE_2 =>
                frame.setLong(frame.resolveLocalIndex(2), frame.popLong());
            case Bytecodes.LSTORE_3 =>
                frame.setLong(frame.resolveLocalIndex(3), frame.popLong());
            case Bytecodes.FSTORE_0 =>
                frame.setFloat(frame.resolveLocalIndex(0), frame.popFloat());
            case Bytecodes.FSTORE_1 =>
                frame.setFloat(frame.resolveLocalIndex(1), frame.popFloat());
            case Bytecodes.FSTORE_2 =>
                frame.setFloat(frame.resolveLocalIndex(2), frame.popFloat());
            case Bytecodes.FSTORE_3 =>
                frame.setFloat(frame.resolveLocalIndex(3), frame.popFloat());
            case Bytecodes.DSTORE_0 =>
                frame.setDouble(frame.resolveLocalIndex(0), frame.popDouble());
            case Bytecodes.DSTORE_1 =>
                frame.setDouble(frame.resolveLocalIndex(1), frame.popDouble());
            case Bytecodes.DSTORE_2 =>
                frame.setDouble(frame.resolveLocalIndex(2), frame.popDouble());
            case Bytecodes.DSTORE_3 =>
                frame.setDouble(frame.resolveLocalIndex(3), frame.popDouble());
            case Bytecodes.ASTORE_0 =>
                frame.setObject(frame.resolveLocalIndex(0), frame.popObject());
            case Bytecodes.ASTORE_1 =>
                frame.setObject(frame.resolveLocalIndex(1), frame.popObject());
            case Bytecodes.ASTORE_2 =>
                frame.setObject(frame.resolveLocalIndex(2), frame.popObject());
            case Bytecodes.ASTORE_3 =>
                frame.setObject(frame.resolveLocalIndex(3), frame.popObject());
            case Bytecodes.IASTORE =>
                runtimeInterface.setArrayInt(frame.popInt(), frame.popInt(), frame.popObject());
            case Bytecodes.LASTORE =>
                runtimeInterface.setArrayLong(frame.popLong(), frame.popInt(), frame.popObject());
            case Bytecodes.FASTORE =>
                runtimeInterface.setArrayFloat(frame.popFloat(), frame.popInt(), frame.popObject());
            case Bytecodes.DASTORE =>
                runtimeInterface.setArrayDouble(frame.popDouble(), frame.popInt(), frame.popObject());
            case Bytecodes.AASTORE =>
                runtimeInterface.setArrayObject(frame.popObject(), frame.popInt(), frame.popObject());
            case Bytecodes.BASTORE =>
                runtimeInterface.setArrayByte(frame.popInt().toByte, frame.popInt(), frame.popObject());
            case Bytecodes.CASTORE =>
                runtimeInterface.setArrayChar(frame.popInt().toChar, frame.popInt(), frame.popObject());
            case Bytecodes.SASTORE =>
                runtimeInterface.setArrayShort(frame.popInt().toShort, frame.popInt(), frame.popObject());
            case Bytecodes.POP =>
                frame.popVoid(1);
            case Bytecodes.POP2 =>
                frame.popVoid(2);
            case Bytecodes.DUP =>
                frame.dup(1);
            case Bytecodes.DUP_X1 =>
                frame.dupx1();
            case Bytecodes.DUP_X2 =>
                frame.dupx2();
            case Bytecodes.DUP2 =>
                frame.dup(2);
            case Bytecodes.DUP2_X1 =>
                frame.dup2x1();
            case Bytecodes.DUP2_X2 =>
                frame.dup2x2();
            case Bytecodes.SWAP =>
                frame.swapSingle();
            case Bytecodes.IADD =>
                frame.pushInt(frame.popInt() + frame.popInt());
            case Bytecodes.LADD =>
                frame.pushLong(frame.popLong() + frame.popLong());
            case Bytecodes.FADD =>
                frame.pushFloat(frame.popFloat() + frame.popFloat());
            case Bytecodes.DADD =>
                frame.pushDouble(frame.popDouble() + frame.popDouble());
            case Bytecodes.ISUB =>
                frame.pushInt(-frame.popInt() + frame.popInt());
            case Bytecodes.LSUB =>
                frame.pushLong(-frame.popLong() + frame.popLong());
            case Bytecodes.FSUB =>
                frame.pushFloat(-frame.popFloat() + frame.popFloat());
            case Bytecodes.DSUB =>
                frame.pushDouble(-frame.popDouble() + frame.popDouble());
            case Bytecodes.IMUL =>
                frame.pushInt(frame.popInt() * frame.popInt());
            case Bytecodes.LMUL =>
                frame.pushLong(frame.popLong() * frame.popLong());
            case Bytecodes.FMUL =>
                frame.pushFloat(frame.popFloat() * frame.popFloat());
            case Bytecodes.DMUL =>
                frame.pushDouble(frame.popDouble() * frame.popDouble());
            case Bytecodes.IDIV =>
                divInt(frame);
            case Bytecodes.LDIV =>
                divLong(frame);
            case Bytecodes.FDIV =>
                divFloat(frame);
            case Bytecodes.DDIV =>
                divDouble(frame);
            case Bytecodes.IREM =>
                remInt(frame);
            case Bytecodes.LREM =>
                remLong(frame);
            case Bytecodes.FREM =>
                remFloat(frame);
            case Bytecodes.DREM =>
                remDouble(frame);
            case Bytecodes.INEG =>
                frame.pushInt(-frame.popInt());
            case Bytecodes.LNEG =>
                frame.pushLong(-frame.popLong());
            case Bytecodes.FNEG =>
                frame.pushFloat(-frame.popFloat());
            case Bytecodes.DNEG =>
                frame.pushDouble(-frame.popDouble());
            case Bytecodes.ISHL =>
                shiftLeftInt(frame);
            case Bytecodes.LSHL =>
                shiftLeftLong(frame);
            case Bytecodes.ISHR =>
                shiftRightSignedInt(frame);
            case Bytecodes.LSHR =>
                shiftRightSignedLong(frame);
            case Bytecodes.IUSHR =>
                shiftRightUnsignedInt(frame);
            case Bytecodes.LUSHR =>
                shiftRightUnsignedLong(frame);
            case Bytecodes.IAND =>
                frame.pushInt(frame.popInt() & frame.popInt());
            case Bytecodes.LAND =>
                frame.pushLong(frame.popLong() & frame.popLong());
            case Bytecodes.IOR =>
                frame.pushInt(frame.popInt() | frame.popInt());
            case Bytecodes.LOR =>
                frame.pushLong(frame.popLong() | frame.popLong());
            case Bytecodes.IXOR =>
                frame.pushInt(frame.popInt() ^ frame.popInt());
            case Bytecodes.LXOR =>
                frame.pushLong(frame.popLong() ^ frame.popLong());
            case Bytecodes.IINC =>
                iinc(frame, bs);
            case Bytecodes.I2L =>
                frame.pushLong(frame.popInt());
            case Bytecodes.I2F =>
                frame.pushFloat(frame.popInt());
            case Bytecodes.I2D =>
                frame.pushDouble(frame.popInt());
            case Bytecodes.L2I =>
                frame.pushInt(frame.popLong().toInt);
            case Bytecodes.L2F =>
                frame.pushFloat(frame.popLong());
            case Bytecodes.L2D =>
                frame.pushDouble(frame.popLong());
            case Bytecodes.F2I =>
                frame.pushInt(frame.popFloat().toInt);
            case Bytecodes.F2L =>
                frame.pushLong(frame.popFloat().toLong);
            case Bytecodes.F2D =>
                frame.pushDouble(frame.popFloat());
            case Bytecodes.D2I =>
                frame.pushInt(frame.popDouble().toInt);
            case Bytecodes.D2L =>
                frame.pushLong(frame.popDouble().toLong);
            case Bytecodes.D2F =>
                frame.pushFloat(frame.popDouble().toFloat);
            case Bytecodes.I2B =>
                frame.pushInt(frame.popInt().toByte);
            case Bytecodes.I2C =>
                frame.pushInt(frame.popInt().toChar);
            case Bytecodes.I2S =>
                frame.pushInt(frame.popInt().toShort);
            case Bytecodes.LCMP =>
                compareLong(frame);
            case Bytecodes.FCMPL =>
                compareFloatLess(frame);
            case Bytecodes.FCMPG =>
                compareFloatGreater(frame);
            case Bytecodes.DCMPL =>
                compareDoubleLess(frame);
            case Bytecodes.DCMPG =>
                compareDoubleGreater(frame);
            case Bytecodes.IFEQ =>
                if (frame.popInt() == 0) {
                    return BRANCH;
                }
            case Bytecodes.IFNE =>
                if (frame.popInt() != 0) {
                    return BRANCH;
                }
            case Bytecodes.IFLT =>
                if (frame.popInt() < 0) {
                    return BRANCH;
                }
            case Bytecodes.IFGE =>
                if (frame.popInt() >= 0) {
                    return BRANCH;
                }
            case Bytecodes.IFGT =>
                if (frame.popInt() > 0) {
                    return BRANCH;
                }
            case Bytecodes.IFLE =>
                if (frame.popInt() <= 0) {
                    return BRANCH;
                }
            case Bytecodes.IF_ICMPEQ =>
                if (frame.popInt() == frame.popInt()) {
                    return BRANCH;
                }
            case Bytecodes.IF_ICMPNE =>
                if (frame.popInt() != frame.popInt()) {
                    return BRANCH;
                }
            case Bytecodes.IF_ICMPLT =>
                if (frame.popInt() > frame.popInt()) {
                    return BRANCH;
                }
            case Bytecodes.IF_ICMPGE =>
                if (frame.popInt() <= frame.popInt()) {
                    return BRANCH;
                }
            case Bytecodes.IF_ICMPGT =>
                if (frame.popInt() < frame.popInt()) {
                    return BRANCH;
                }
            case Bytecodes.IF_ICMPLE =>
                if (frame.popInt() >= frame.popInt()) {
                    return BRANCH;
                }
            case Bytecodes.IF_ACMPEQ =>
                if (frame.popObject() == frame.popObject()) {
                    return BRANCH;
                }
            case Bytecodes.IF_ACMPNE =>
                if (frame.popObject() != frame.popObject()) {
                    return BRANCH;
                }
            case Bytecodes.GOTO | Bytecodes.GOTO_W =>
                return BRANCH;
            case Bytecodes.JSR | Bytecodes.JSR_W =>
                frame.pushInt(bs.currentBCI());
                return BRANCH;
            case Bytecodes.RET =>
                return frame.getInt(frame.resolveLocalIndex(bs.readLocalIndex()));
            case Bytecodes.TABLESWITCH =>
                return tableSwitch(frame, bs);
            case Bytecodes.LOOKUPSWITCH =>
                return lookupSwitch(frame, bs);
            case Bytecodes.IRETURN =>
                frame.getParentFrame().pushInt(frame.popInt());
                return RETURN;
            case Bytecodes.LRETURN =>
                frame.getParentFrame().pushLong(frame.popLong());
                return RETURN;
            case Bytecodes.FRETURN =>
                frame.getParentFrame().pushFloat(frame.popFloat());
                return RETURN;
            case Bytecodes.DRETURN =>
                frame.getParentFrame().pushDouble(frame.popDouble());
                return RETURN;
            case Bytecodes.ARETURN =>
                frame.getParentFrame().pushObject(frame.popObject());
                return RETURN;
            case Bytecodes.RETURN =>
                return RETURN;
            case Bytecodes.GETSTATIC =>
                getField(frame, null, bs.currentBC(), bs.readCPI());
            case Bytecodes.PUTSTATIC =>
                putStatic(frame, bs.readCPI());
            case Bytecodes.GETFIELD =>
                getField(frame, nullCheck(frame.popObject()), bs.currentBC(), bs.readCPI());
            case Bytecodes.PUTFIELD =>
                putField(frame, bs.readCPI());
            case Bytecodes.INVOKEVIRTUAL =>
                callFrame = invokeVirtual(frame, bs.readCPI());
                if (callFrame != null) {
                    return CALL
                }
            case Bytecodes.INVOKESPECIAL =>
                callFrame = invokeSpecial(frame, bs.readCPI());
                if (callFrame != null) {
                    return CALL
                }
            case Bytecodes.INVOKESTATIC =>
                callFrame = invokeStatic(frame, bs.readCPI());
                if (callFrame != null) {
                    return CALL
                }
            case Bytecodes.INVOKEINTERFACE =>
                callFrame = invokeInterface(frame, bs.readCPI());
                if (callFrame != null) {
                    return CALL
                }
            case Bytecodes.XXXUNUSEDXXX =>
                assert(false, "unused bytecode used. behaviour unspecified.");
                // nop
            case Bytecodes.NEW =>
                frame.pushObject(allocateInstance(frame, bs.readCPI()));
            case Bytecodes.NEWARRAY =>
                frame.pushObject(allocateNativeArray(frame, bs.readByte()));
            case Bytecodes.ANEWARRAY =>
                frame.pushObject(allocateArray(frame, bs.readCPI()));
            case Bytecodes.ARRAYLENGTH =>
                frame.pushInt(java.lang.reflect.Array.getLength(nullCheck(frame.popObject())));
            case Bytecodes.ATHROW =>
                val t = frame.popObject().asInstanceOf[Throwable];
                if ("break".equals(t.getMessage())) {
                    t.printStackTrace();
                }
                throw t;
            case Bytecodes.CHECKCAST =>
                checkCast(frame, bs.readCPI());
            case Bytecodes.INSTANCEOF =>
                instanceOf(frame, bs.readCPI());
            case Bytecodes.MONITORENTER =>
                runtimeInterface.monitorEnter(frame.popObject());
            case Bytecodes.MONITOREXIT =>
                runtimeInterface.monitorExit(frame.popObject());
            case Bytecodes.WIDE =>
                assert(false);
            case Bytecodes.MULTIANEWARRAY =>
                frame.pushObject(allocateMultiArray(frame, bs.readCPI(), bs.readUByte(bs.currentBCI() + 3)));
            case Bytecodes.IFNULL =>
                if (frame.popObject() == null) {
                    return BRANCH;
                }
            case Bytecodes.IFNONNULL =>
                if (frame.popObject() != null) {
                    return BRANCH;
                }
            case Bytecodes.BREAKPOINT =>
                assert(false, "no breakpoints supported at this time.");
        }
        return NEXT;
    }

    private def handleThrowable(root: InterpreterFrame, frame: InterpreterFrame, t: Throwable): InterpreterFrame = {
        var handler: ExceptionHandler = null;
        var currentFrame: InterpreterFrame = frame;
        do {
            handler = resolveExceptionHandlers(currentFrame, currentFrame.getBCI(), t);
            if (handler == null) {
                // no handler found pop frame
                // and continue searching
                currentFrame = popFrame(currentFrame);
            } else {
                // found a handler -> execute it
                currentFrame.setBCI(handler.handlerBCI());
                currentFrame.popStack();
                currentFrame.pushObject(t);
                return currentFrame;
            }
        } while (handler == null && currentFrame != root);

        // will throw exception up the interpreter
        return null;
    }

    private def updateStackTrace(frame: InterpreterFrame, t: Throwable) {
        val elements: Array[StackTraceElement] = getBackTrace(frame, t);
        if (elements != null) {
            setStackTrace(frame, t, elements);
            setBackTrace(frame, t, null);
        } else {
            setBackTrace(frame, t, createStackTraceElements(frame));
        }
    }

    private def setStackTrace(frame: InterpreterFrame, t: Throwable, stackTrace: Array[StackTraceElement]): Unit = {
        runtimeInterface.setFieldObject(stackTrace, t, findThrowableField(frame, "stackTrace"));
    }

    private def getBackTrace(frame: InterpreterFrame, t: Throwable): Array[StackTraceElement] = {
        val value: Object = runtimeInterface.getFieldObject(t, findThrowableField(frame, "backtrace"));
        if (value.isInstanceOf[Array[StackTraceElement]]) {
            return value.asInstanceOf[Array[StackTraceElement]];
        }
        return null;
    }

    private def setBackTrace(frame: InterpreterFrame, t: Throwable, backtrace: Array[StackTraceElement]): Unit = {
        runtimeInterface.setFieldObject(backtrace, t, findThrowableField(frame, "backtrace"));
    }

    private def resolveExceptionHandlers(frame: InterpreterFrame, bci: Int, t: Throwable): ExceptionHandler = {
        val handlers: Array[ExceptionHandler] = frame.getMethod().exceptionHandlers();
        var i = 0
        while (i < handlers.length) {
            val handler: ExceptionHandler = handlers(i);
            if (bci >= handler.startBCI() && bci <= handler.endBCI()) {
                var catchType: ResolvedJavaType = null;
                if (!handler.isCatchAll()) {
                    // exception handlers are similar to instanceof bytecodes, so we pass instanceof
                    catchType = resolveType(frame, Bytecodes.INSTANCEOF, handler.catchTypeCPI().toChar);
                }

                if (catchType == null || catchType.toJava().isInstance(t)) {
                    // the first found exception handler is our exception handler
                    return handler;
                }
            }
            i += 1
        }
        return null;
    }

    private def allocateFrame(frame: InterpreterFrame, bs: BytecodeStream): InterpreterFrame = {
        try {
            val nextFrame: InterpreterFrame = this.callFrame;

            assert(nextFrame != null);
            assert(nextFrame.getParentFrame() == frame);

            // store bci when leaving method
            frame.setBCI(bs.currentBCI());

            if (TRACE) {
                traceCall(nextFrame, "Call");
            }
            if (Modifier.isSynchronized(nextFrame.getMethod().accessFlags())) {
                if (TRACE) {
                    traceOp(frame, "Method monitor enter");
                }
                if (Modifier.isStatic(nextFrame.getMethod().accessFlags())) {
                    runtimeInterface.monitorEnter(nextFrame.getMethod().holder().toJava());
                } else {
                    val enterObject: Object = nextFrame.getObject(frame.resolveLocalIndex(0));
                    assert(enterObject != null);
                    runtimeInterface.monitorEnter(enterObject);
                }
            }

            return nextFrame;
        } finally {
            callFrame = null;
            bs.next();
        }
    }

    private def popFrame(frame: InterpreterFrame): InterpreterFrame = {
        val parent: InterpreterFrame = frame.getParentFrame();
        if (Modifier.isSynchronized(frame.getMethod().accessFlags())) {
            if (TRACE) {
                traceOp(frame, "Method monitor exit");
            }
            if (Modifier.isStatic(frame.getMethod().accessFlags())) {
                runtimeInterface.monitorExit(frame.getMethod().holder().toJava());
            } else {
                val exitObject: Object = frame.getObject(frame.resolveLocalIndex(0));
                if (exitObject != null) {
                    runtimeInterface.monitorExit(exitObject);
                }
            }
        }
        if (TRACE) {
            traceCall(frame, "Ret");
        }

        frame.dispose();
        return parent;
    }

    private def traceOp(frame: InterpreterFrame, opName: String): Unit = {
        trace(frame.depth(), opName);
    }

    private def traceCall(frame: InterpreterFrame, typ: String) {
        trace(frame.depth(), typ + " " + frame.getMethod() + " - " + frame.getMethod().signature().asString());
    }

    private def trace(level: Int, message: String) {
        val builder = new StringBuilder();
        var i = 0
        while (i < level) {
            builder.append("  ");
            i += 1
        }
        builder.append(message);
        System.out.println(builder);
    }

    private def divInt(frame: InterpreterFrame) {
        val dividend = frame.popInt();
        val divisor = frame.popInt();
        frame.pushInt(divisor / dividend);
    }

    private def divLong(frame: InterpreterFrame) {
        val dividend = frame.popLong();
        val divisor = frame.popLong();
        frame.pushLong(divisor / dividend);
    }

    private def divFloat(frame: InterpreterFrame) {
        val dividend = frame.popFloat();
        val divisor = frame.popFloat();
        frame.pushFloat(divisor / dividend);
    }

    private def divDouble(frame: InterpreterFrame) {
        val dividend = frame.popDouble();
        val divisor = frame.popDouble();
        frame.pushDouble(divisor / dividend);
    }

    private def remInt(frame: InterpreterFrame) {
        val dividend = frame.popInt();
        val divisor = frame.popInt();
        frame.pushInt(divisor % dividend);
    }

    private def remLong(frame: InterpreterFrame) {
        val dividend = frame.popLong();
        val divisor = frame.popLong();
        frame.pushLong(divisor % dividend);
    }

    private def remFloat(frame: InterpreterFrame) {
        val dividend = frame.popFloat();
        val divisor = frame.popFloat();
        frame.pushFloat(divisor % dividend);
    }

    private def remDouble(frame: InterpreterFrame) {
        val dividend = frame.popDouble();
        val divisor = frame.popDouble();
        frame.pushDouble(divisor % dividend);
    }

    private def shiftLeftInt(frame: InterpreterFrame) {
        val bits = frame.popInt();
        val value = frame.popInt();
        frame.pushInt(value << bits);
    }

    private def shiftLeftLong(frame: InterpreterFrame) {
        val bits = frame.popInt();
        val value = frame.popLong();
        frame.pushLong(value << bits);
    }

    private def shiftRightSignedInt(frame: InterpreterFrame) {
        val bits = frame.popInt();
        val value = frame.popInt();
        frame.pushInt(value >> bits);
    }

    private def shiftRightSignedLong(frame: InterpreterFrame) {
        val bits = frame.popInt();
        val value = frame.popLong();
        frame.pushLong(value >> bits);
    }

    private def shiftRightUnsignedInt(frame: InterpreterFrame) {
        val bits = frame.popInt();
        val value = frame.popInt();
        frame.pushInt(value >>> bits);
    }

    private def shiftRightUnsignedLong(frame: InterpreterFrame) {
        val bits = frame.popInt();
        val value = frame.popLong();
        frame.pushLong(value >>> bits);
    }

    private def lookupSwitch(frame: InterpreterFrame, bs: BytecodeStream): Int = {
        return lookupSearch(new BytecodeLookupSwitch(bs, bs.currentBCI()), frame.popInt());
    }

    /**
     * Binary search implementation for the lookup switch.
     */
    private def lookupSearch(switchHelper: BytecodeLookupSwitch, key: Int): Int = {
        var low = 0;
        var high = switchHelper.numberOfCases() - 1;
        while (low <= high) {
            val mid = (low + high) >>> 1;
            val midVal = switchHelper.keyAt(mid);

            if (midVal < key) {
                low = mid + 1;
            } else if (midVal > key) {
                high = mid - 1;
            } else {
                return switchHelper.bci() + switchHelper.offsetAt(mid); // key found
            }
        }
        return switchHelper.defaultTarget(); // key not found.
    }

    private def tableSwitch(frame: InterpreterFrame, bs: BytecodeStream): Int = {
        val switchHelper = new BytecodeTableSwitch(bs, bs.currentBCI());

        val low = switchHelper.lowKey();
        val high = switchHelper.highKey();

        assert(low <= high);

        val index = frame.popInt();
        if (index < low || index > high) {
            return switchHelper.defaultTarget();
        } else {
            return switchHelper.targetAt(index - low);
        }
    }

    private def checkCast(frame: InterpreterFrame, cpi: Char): Unit = {
        val typ = resolveType(frame, Bytecodes.CHECKCAST, cpi).toJava()
        frame.pushObject(typ.cast(frame.popObject()).asInstanceOf[Object]);
    }

    private def resolveType(frame: InterpreterFrame, opcode: Int, cpi: Char): ResolvedJavaType = {
        val constantPool: ConstantPool = frame.getConstantPool();
        constantPool.loadReferencedType(cpi, opcode);
        return constantPool.lookupType(cpi, opcode).resolve(frame.getMethod().holder());
    }

    private def resolveType(frame: InterpreterFrame, javaClass: Class[_]): ResolvedJavaType = {
        return metaAccessProvider.getResolvedJavaType(javaClass).resolve(frame.getMethod().holder());
    }

    private def resolveMethod(frame: InterpreterFrame, opcode: Int, cpi: Char): ResolvedJavaMethod = {
        val constantPool: ConstantPool = frame.getConstantPool();
        constantPool.loadReferencedType(cpi, opcode);
        return constantPool.lookupMethod(cpi, opcode).asInstanceOf[ResolvedJavaMethod];
    }

    private def resolveField(frame: InterpreterFrame, opcode: Int, cpi: Char): ResolvedJavaField = {
        val constantPool: ConstantPool = frame.getConstantPool();
        constantPool.loadReferencedType(cpi, opcode);
        return constantPool.lookupField(cpi, opcode).asInstanceOf[ResolvedJavaField];
    }

    private def instanceOf(frame: InterpreterFrame, cpi: Char): Unit = {
        frame.pushInt(if (resolveType(frame, Bytecodes.INSTANCEOF, cpi).toJava().isInstance(frame.popObject())) 1 else 0);
    }

    private def pushCPConstant(frame: InterpreterFrame, cpi: Char): Unit = {
        val method: ResolvedJavaMethod = frame.getMethod();
        val constant: Object = method.getConstantPool().lookupConstant(cpi);

        if (constant.isInstanceOf[Constant]) {
            val c: Constant = constant.asInstanceOf[Constant]
            c.kind match {
                case Kind.Int =>
                    frame.pushInt(c.asInt());
                case Kind.Float =>
                    frame.pushFloat(c.asFloat());
                case Kind.Object =>
                    frame.pushObject(c.asObject());
                case Kind.Double =>
                    frame.pushDouble(c.asDouble());
                case Kind.Long =>
                    frame.pushLong(c.asLong());
                case _ =>
                    assert(false, "unspecified case")
            }
        } else if (constant.isInstanceOf[JavaType]) {
            frame.pushObject((constant.asInstanceOf[JavaType]).resolve(method.holder()).toJava());
        } else {
            assert(false, "unexpected case");
        }
    }

    private def compareLong(frame: InterpreterFrame) {
        val y = frame.popLong();
        val x = frame.popLong();
        frame.pushInt(if (x < y) -1 else if (x == y) 0 else 1);
    }

    private def compareDoubleGreater(frame: InterpreterFrame) {
        val y = frame.popDouble();
        val x = frame.popDouble();
        frame.pushInt(if (x < y) -1 else if (x == y) 0 else 1);
    }

    private def compareDoubleLess(frame: InterpreterFrame) {
        val y = frame.popDouble();
        val x = frame.popDouble();
        frame.pushInt(if (x > y) 1 else if (x == y) 0 else -1);
    }

    private def compareFloatGreater(frame: InterpreterFrame) {
        val y = frame.popFloat();
        val x = frame.popFloat();
        frame.pushInt(if (x < y) -1 else if (x == y) 0 else 1);
    }

    private def compareFloatLess(frame: InterpreterFrame) {
        val y = frame.popFloat();
        val x = frame.popFloat();
        frame.pushInt(if (x > y) 1 else if (x == y) 0 else -1);
    }

    private def nullCheck(value: Object): Object = {
        if (value == null) {
            throw new NullPointerException();
        }
        return value;
    }

    private def invokeStatic(frame: InterpreterFrame, cpi: Char): InterpreterFrame = {// throws Throwable {
        return invoke(frame, resolveMethod(frame, Bytecodes.INVOKESTATIC, cpi), null);
    }

    private def invokeInterface(frame: InterpreterFrame, cpi: Char): InterpreterFrame = {// throws Throwable {
        return resolveAndInvoke(frame, resolveMethod(frame, Bytecodes.INVOKEINTERFACE, cpi));
    }

    private def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame = {// throws Throwable {
        val receiver: Object = nullCheck(parent.peekReceiver(m));

        val method: ResolvedJavaMethod = resolveType(parent, receiver.getClass()).resolveMethodImpl(m);

        if (method == null) {
            throw new AbstractMethodError();
        }

        return invoke(parent, method, receiver);
    }

    private def invokeVirtual(frame: InterpreterFrame, cpi: Char): InterpreterFrame = {// throws Throwable {
        val m: ResolvedJavaMethod = resolveMethod(frame, Bytecodes.INVOKEVIRTUAL, cpi);
        if (Modifier.isFinal(m.accessFlags())) {
            return invoke(frame, m, nullCheck(frame.peekReceiver(m)));
        } else {
            return resolveAndInvoke(frame, m);
        }
    }

    private def invokeSpecial(frame: InterpreterFrame, cpi: Char): InterpreterFrame = {// throws Throwable {
        val m: ResolvedJavaMethod = resolveMethod(frame, Bytecodes.INVOKESPECIAL, cpi);
        return invoke(frame, m, nullCheck(frame.peekReceiver(m)));
    }

    private def popArgumentsAsObject(frame: InterpreterFrame, method: ResolvedJavaMethod, hasReceiver: Boolean): Array[Object] = {
        val signature: Signature = method.signature();
        val argumentCount: Int = method.signature().argumentCount(hasReceiver);
        val parameters: Array[Object] = new Array[Object](argumentCount);

        val lastSignatureIndex: Int = if (hasReceiver) 1 else 0;
        var i = argumentCount - 1
        while (i >= lastSignatureIndex) {
            val typ: ResolvedJavaType = signature.argumentTypeAt(i - lastSignatureIndex, method.holder()).resolve(method.holder());
            parameters(i) = popAsObject(frame, typ.kind());
            i -= 1
        }

        if (hasReceiver) {
            parameters(0) = frame.popObject();
        }
        return parameters;
    }

    private def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, receiver: Object): InterpreterFrame = {// throws Throwable {
        if (caller.depth() >= maxStackFrames) {
            throw new StackOverflowError("Maximum callstack of " + maxStackFrames + " exceeded.");
        }

        if (Modifier.isNative(method.accessFlags())) {
            return invokeNativeMethodViaVM(caller, method, receiver != null);
        } else {
            val redirectedMethod: MethodRedirectionInfo = methodDelegates.get(method);
            if (redirectedMethod != null) {
                return invokeRedirectedMethodViaVM(caller, method, redirectedMethod, receiver != null);
            } else {
                return invokeOptimized(caller, method, receiver != null);
            }
        }
    }

    private def invokeNativeMethodViaVM(caller: InterpreterFrame, method: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame = {// throws Throwable {
        assert(!methodDelegates.containsKey(method), "must not be redirected");
        if (TRACE) {
            traceCall(caller, "Native " + method);
        }

        // mark the current thread as high level and execute the native method
        val parameters: Array[Object] = popArgumentsAsObject(caller, method, hasReceiver);
        val returnValue: Object = runtimeInterface.invoke(method, parameters);
        pushAsObject(caller, method.signature().returnKind(), returnValue);

        return null;
    }

    private def invokeRedirectedMethodViaVM(caller: InterpreterFrame, originalMethod: ResolvedJavaMethod, redirectionInfo: MethodRedirectionInfo, hasReceiver: Boolean): InterpreterFrame = {// throws Throwable {
        assert(methodDelegates.containsKey(originalMethod), "must be redirected");
        if (TRACE) {
            traceCall(caller, "Delegate " + originalMethod);
        }

        // current thread is low level and we also execute the target method in the low-level interpreter
        val originalCalleeParameters: Array[Object] = popArgumentsAsObject(caller, originalMethod, hasReceiver);
        val parameters: Array[Object] = Array[Object](caller, originalMethod, originalCalleeParameters);
        val returnValue: Object = redirectionInfo.getTargetMethod().invoke(redirectionInfo.getReceiver(), parameters);
        pushAsObject(caller, originalMethod.signature().returnKind(), returnValue);

        return null;
    }

    private def invokeOptimized(parent: InterpreterFrame, method: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame = {// throws Throwable {
        //return parent.create(method, hasReceiver);
        return parent.create(method, hasReceiver, 0, true);
    }

    /*
    private void enterMethod(InterpreterFrame calleeFrame) {
        ResolvedJavaMethod method = calleeFrame.getMethod();
        if (TRACE) {
            traceCall(calleeFrame, "Call");
        }

        if (Modifier.isSynchronized(method.accessFlags())) {
            if (TRACE) {
                traceOp(calleeFrame, "Method monitor enter");
            }
            if (Modifier.isStatic(method.accessFlags())) {
                runtimeInterface.monitorEnter(method.holder().toJava());
            } else {
                Object enterObject = calleeFrame.getObject(calleeFrame.resolveLocalIndex(0));
                assert enterObject != null;
                runtimeInterface.monitorEnter(enterObject);
            }
        }
    }*/



    private def allocateMultiArray(frame: InterpreterFrame, cpi: Char, dimension: Int): Object = {
        val typ: ResolvedJavaType = getLastDimensionType(resolveType(frame, Bytecodes.MULTIANEWARRAY, cpi));

        val dimensions = new Array[Int](dimension);
        var i = dimension - 1
        while (i >= 0) {        
            dimensions(i) = frame.popInt();
            i -= 1
        }
        return java.lang.reflect.Array.newInstance(typ.toJava(), dimensions:_*);
    }

    private def getLastDimensionType(typ: ResolvedJavaType): ResolvedJavaType = {
        var result: ResolvedJavaType = typ;
        while (result.isArrayClass()) {
            result = result.componentType();
        }
        return result;
    }

    private def allocateArray(frame: InterpreterFrame, cpi: Char): Object = {
        val typ: ResolvedJavaType = resolveType(frame, Bytecodes.ANEWARRAY, cpi);
        return java.lang.reflect.Array.newInstance(typ.toJava(), frame.popInt());
    }

    private def allocateNativeArray(frame: InterpreterFrame, cpi: Byte): Object = {
        // the constants for the cpi are loosely defined and no real cpi indices.
        cpi match {
            case 4 =>
                return new Array[Byte](frame.popInt());
            case 8 =>
                return new Array[Byte](frame.popInt());
            case 5 =>
                return new Array[Char](frame.popInt());
            case 7 =>
                return new Array[Double](frame.popInt());
            case 6 =>
                return new Array[Float](frame.popInt());
            case 10 =>
                return new Array[Int](frame.popInt());
            case 11 =>
                return new Array[Long](frame.popInt());
            case 9 =>
                return new Array[Short](frame.popInt());
            case _ =>
                assert(false, "unexpected case");
                return null;
        }
    }

    private def allocateInstance(frame: InterpreterFrame, cpi: Char): Object = {// throws InstantiationException {
        return runtimeInterface.newObject(resolveType(frame, Bytecodes.NEW, cpi));
    }

    private def iinc(frame: InterpreterFrame, bs: BytecodeStream): Unit = {
        val index: Int = frame.resolveLocalIndex(bs.readLocalIndex());
        frame.setInt(index, frame.getInt(index) + bs.readIncrement());
    }

    private def putStatic(frame: InterpreterFrame, cpi: Char): Unit = {
        putFieldStatic(frame, resolveField(frame, Bytecodes.PUTSTATIC, cpi));
    }

    private def putField(frame: InterpreterFrame, cpi: Char): Unit = {
        putFieldVirtual(frame, resolveField(frame, Bytecodes.PUTFIELD, cpi));
    }

    private def putFieldStatic(frame: InterpreterFrame, field: ResolvedJavaField): Unit = {
        field.kind() match {
            case Kind.Boolean | Kind.Byte | Kind.Char | Kind.Short | Kind.Int =>
                runtimeInterface.setFieldInt(frame.popInt(), null, field);                
            case Kind.Double =>
                runtimeInterface.setFieldDouble(frame.popDouble(), null, field);
            case Kind.Float =>
                runtimeInterface.setFieldFloat(frame.popFloat(), null, field);
            case Kind.Long =>
                runtimeInterface.setFieldLong(frame.popLong(), null, field);
            case Kind.Object =>
                runtimeInterface.setFieldObject(frame.popObject(), null, field);
            case _ =>
                assert(false, "unexpected case");
        }
    }

    private def putFieldVirtual(frame: InterpreterFrame, field: ResolvedJavaField): Unit = {
        field.kind() match {
            case Kind.Boolean | Kind.Byte | Kind.Char | Kind.Short | Kind.Int =>
                runtimeInterface.setFieldInt(frame.popInt(), nullCheck(frame.popObject()), field);
            case Kind.Double =>
                runtimeInterface.setFieldDouble(frame.popDouble(), nullCheck(frame.popObject()), field);
            case Kind.Float =>
                runtimeInterface.setFieldFloat(frame.popFloat(), nullCheck(frame.popObject()), field);
            case Kind.Long =>
                runtimeInterface.setFieldLong(frame.popLong(), nullCheck(frame.popObject()), field);
            case Kind.Object =>
                runtimeInterface.setFieldObject(frame.popObject(), nullCheck(frame.popObject()), field);
            case _ =>
                assert(false, "unexpected case");
        }
    }

    private def getField(frame: InterpreterFrame, base: Object, opcode: Int, cpi: Char): Unit = {
        val field: ResolvedJavaField = resolveField(frame, opcode, cpi);
        field.kind() match {
            case Kind.Boolean =>
                frame.pushInt(if (runtimeInterface.getFieldBoolean(base, field)) 1 else 0);
            case Kind.Byte =>
                frame.pushInt(runtimeInterface.getFieldByte(base, field));
            case Kind.Char =>
                frame.pushInt(runtimeInterface.getFieldChar(base, field));
            case Kind.Short =>
                frame.pushInt(runtimeInterface.getFieldShort(base, field));
            case Kind.Int =>
                frame.pushInt(runtimeInterface.getFieldInt(base, field));
            case Kind.Double =>
                frame.pushDouble(runtimeInterface.getFieldDouble(base, field));
            case Kind.Float =>
                frame.pushFloat(runtimeInterface.getFieldFloat(base, field));
            case Kind.Long =>
                frame.pushLong(runtimeInterface.getFieldLong(base, field));
            case Kind.Object =>
                frame.pushObject(runtimeInterface.getFieldObject(base, field));
            case _ =>
                assert(false, "unexpected case");
        }
    }

    private def pushAsObject(frame: InterpreterFrame, typeKind: Kind, value: Object): Int = {
        typeKind match {
            case Kind.Int =>
                frame.pushInt(value.asInstanceOf[Int]);
            case Kind.Long =>
                frame.pushLong(value.asInstanceOf[Long]);
                return 2;
            case Kind.Boolean =>
                frame.pushInt(if (value.asInstanceOf[Boolean]) 1 else 0);
            case Kind.Byte =>
                frame.pushInt(value.asInstanceOf[Byte]);
            case Kind.Char =>
                frame.pushInt(value.asInstanceOf[Char]);
            case Kind.Double =>
                frame.pushDouble(value.asInstanceOf[Double]);
                return 2;
            case Kind.Float =>
                frame.pushFloat(value.asInstanceOf[Float]);
            case Kind.Short =>
                frame.pushInt(value.asInstanceOf[Short]);
            case Kind.Object =>
                frame.pushObject(value);
            case Kind.Void =>
                return 0;
            case _ =>
                assert(false, "case not specified");
        }
        return 1;
    }

    private def popAsObject(frame: InterpreterFrame, typeKind: Kind): Object = {
        typeKind match {
            case Kind.Boolean =>
                return (frame.popInt() == 1): java.lang.Boolean
            case Kind.Byte =>
                return frame.popInt().toByte: java.lang.Byte;
            case Kind.Char =>
                return frame.popInt().toChar: java.lang.Character;
            case Kind.Double =>
                return frame.popDouble(): java.lang.Double;
            case Kind.Int =>
                return frame.popInt(): java.lang.Integer;
            case Kind.Float =>
                return frame.popFloat(): java.lang.Float;
            case Kind.Long =>
                return frame.popLong(): java.lang.Long;
            case Kind.Short =>
                return frame.popInt().toShort: java.lang.Short;
            case Kind.Object =>
                return frame.popObject();
            case Kind.Void =>
                return null;
            case _ =>
                assert(false, "unexpected case")
        }
        return null;
    }

    private def resolveRootMethod(): ResolvedJavaMethod = {
        try {
            return metaAccessProvider.getResolvedJavaMethod(classOf[BytecodeInterpreter].getDeclaredMethod("execute", classOf[Method], classOf[Array[Object]]));
        } catch {
            case e: Exception =>
            throw new RuntimeException(e);
        }
    }

    private def findMethod(clazz: Class[_], name: String, parameters: Class[_]*): Method = {
        try {
            return clazz.getDeclaredMethod(name, parameters:_*);
        } catch {
            case e: Exception =>
            throw new RuntimeException(e);
        }
    }

    private def createStackTraceElements(frame: InterpreterFrame): Array[StackTraceElement] = {
        var tmp: InterpreterFrame = frame;
        val elements: List[StackTraceElement] = new ArrayList
        var first = false; // filter only first stack elements
        while (tmp != null) {
            if (first || !filterStackElement(tmp)) {
                first = true;
                elements.add(tmp.getMethod().toStackTraceElement(tmp.getBCI()));
            }
            tmp = tmp.getParentFrame();
        }
        return elements.toArray(new Array[StackTraceElement](elements.size()));
    }

    private def filterStackElement(frame: InterpreterFrame): Boolean = {
        return classOf[Throwable].isAssignableFrom(frame.getMethod().holder().toJava());
    }

    private def findThrowableField(frame: InterpreterFrame, name: String): ResolvedJavaField = {
        val throwableType: ResolvedJavaType = resolveType(frame, classOf[Throwable]);
        val fields: Array[ResolvedJavaField] = throwableType.declaredFields();
        var i = 0
        while (i < fields.length) {
            if (fields(i).name().equals(name)) {
                return fields(i);
            }
            i += 1
        }
        assert(false);
        return null;
    }

    private class MethodRedirectionInfo(receiver: InterpreterCallable) {

        private val method: Method = resolveMethod(receiver)

        def getReceiver(): InterpreterCallable = {
            return receiver;
        }

        def getTargetMethod(): Method = {
            return method;
        }

        private def resolveMethod(instance: InterpreterCallable): Method = {
            try {
                return instance.getClass().getMethod(InterpreterCallable.INTERPRETER_CALLABLE_INVOKE_NAME, InterpreterCallable.INTERPRETER_CALLABLE_INVOKE_SIGNATURE:_*);
            } catch {
                case e: NoSuchMethodException =>
                    throw new InterpreterException(e);
                case e: SecurityException =>
                    throw new InterpreterException(e);
            }
        }
    }
}
