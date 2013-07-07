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





trait InterpreterUniverse_Exec extends RuntimeUniverse_Exec with InterpreterUniverse { univ =>



object Frame extends HasUnsafe {
    final val EMPTY_ARRAY = new Array[Object](0)
    final val PARENT_FRAME_SLOT = 0;
    final val MIN_FRAME_SIZE = 1;
}



class Frame_Exec1(numLocals: Int, parent: Frame) extends Frame { // avoid boxing for primitives

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

    def setParentFrame(level: Int, frame: Frame): Unit = {
        assert(level >= 0);
        if (level == 1) {
            setObject(PARENT_FRAME_SLOT, frame);
        } else {
            getObject(PARENT_FRAME_SLOT).asInstanceOf[Frame].setParentFrame(level - 1, frame);
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



class Frame_Exec(numLocals: Int, parent: Frame) extends Frame {
    import Frame._
    assert(numLocals >= MIN_FRAME_SIZE);

    val locals: Array[Object] = new Array[Object](numLocals)

    locals(PARENT_FRAME_SLOT) = parent

    def this(numLocals: Int) = this(numLocals, null);

    def getObject(index: Int): Object = {
        locals(index);
    }

    def setObject(index: Int, value: Object): Unit = {
        locals(index) = value;
    }

    def getFloat(index: Int): Float = {
        locals(index).asInstanceOf[Float]
    }

    def setFloat(index: Int, value: Float): Unit = {
        locals(index) = value.asInstanceOf[AnyRef]
    }

    def getLong(index: Int): Long = {
        locals(index).asInstanceOf[Long]
    }

    def setLong(index: Int, value: Long): Unit = {
        locals(index) = value.asInstanceOf[AnyRef]
    }

    def getInt(index: Int): Int = {
        locals(index).asInstanceOf[Int]
    }

    def setInt(index: Int, value: Int): Unit = {
        locals(index) = value.asInstanceOf[AnyRef]
    }

    def getDouble(index: Int): Double = {
        locals(index).asInstanceOf[Double]
    }

    def setDouble(index: Int, value: Double): Unit = {
        locals(index) = value.asInstanceOf[AnyRef]
    }

    def getParentFrame(level: Int): Frame = {
        assert(level >= 0);
        if (level == 0) {
            return this;
        } else {
            return getObject(PARENT_FRAME_SLOT).asInstanceOf[Frame].getParentFrame(level - 1);
        }
    }

    def setParentFrame(level: Int, frame: Frame): Unit = {
        assert(level >= 0);
        if (level == 1) {
            setObject(PARENT_FRAME_SLOT, frame);
        } else {
            getObject(PARENT_FRAME_SLOT).asInstanceOf[Frame].setParentFrame(level - 1, frame);
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


class InterpreterFrame_Exec1(var method: ResolvedJavaMethod, parent: InterpreterFrame, additionalStackSpace: Int) 
extends Frame_Exec1(method.getMaxLocals() + method.getMaxStackSize() + InterpreterFrame.BASE_LENGTH + additionalStackSpace, parent) 
with InterpreterFrame {

    import Frame._
    import InterpreterFrame._

    var bci: Int = _
    var nextBci: Int = _
    var returnValue: Object = _ // TODO: map to stack

    assert(additionalStackSpace >= 0);

    setMethod(method);
    setBCI(0);
    setNextBCI(-1);
    

    /** Pointer to the top-most stack frame element. */
    private var tos: Int = BASE_LENGTH;

    def this(method: ResolvedJavaMethod, additionalStackSpace: Int) {
        this(method, null, additionalStackSpace);
    }

    def create(method: ResolvedJavaMethod, hasReceiver: Boolean, additionalStackSpace: Int, useParentArguments: Boolean): InterpreterFrame = {
        val frame = new InterpreterFrame_Exec1(method, this, additionalStackSpace);

        if (useParentArguments) {
            val length = method.getSignature().getParameterSlots(hasReceiver);
            assert(length >= 0);

            frame.pushVoid(method.getMaxLocals());
            if (length > 0) {
                copyArguments(frame, length);
                popVoid(length);
            }
        }

        return frame;
    }

    def copy = this

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
        return BASE_LENGTH + getMethod().getMaxLocals();
    }

    private def copyArguments(dest: InterpreterFrame_Exec1, length: Int): Unit = {
        System.arraycopy(locals, tosSingle(length - 1), dest.locals, BASE_LENGTH, length);
        System.arraycopy(primitiveLocals, tosSingle(length - 1), dest.primitiveLocals, BASE_LENGTH, length);
    }


    def getReturnValue(): Object = {
      returnValue
    }
    def setReturnValueObject(value: Object): Unit = {
      returnValue = value
    }
    def setReturnValueInt(value: Int): Unit = {
      returnValue = value: java.lang.Integer
    }
    def setReturnValueLong(value: Long): Unit = {
      returnValue = value: java.lang.Long
    }
    def setReturnValueFloat(value: Float): Unit = {
      returnValue = value: java.lang.Float
    }
    def setReturnValueDouble(value: Double): Unit = {
      returnValue = value: java.lang.Double
    }

    def peekReceiver(method: ResolvedJavaMethod): Object = {
        return getObject(tosSingle(method.getSignature().getParameterSlots(false)));
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
        this.method = method
    }

    def getMethod(): ResolvedJavaMethod = {
        return method
    }

    def setBCI(bci: Int): Unit = {
        this.bci = bci
    }

    def getBCI(): Int = {
        return bci
    }

    def setNextBCI(bci: Int): Unit = {
        this.nextBci = bci
    }

    def getNextBCI(): Int = {
        return nextBci
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

    def setParentFrame(frame: InterpreterFrame): Unit = {
        setObject(PARENT_FRAME_SLOT, frame);
    }

    def dispose(): Unit = {
        // Clear out references in locals array.
        Arrays.fill(locals, null);
    }

    override def toString(): String = {
        val method = getMethod();
        val b = new StringBuilder(getMethod().asStackTraceElement(getBCI()).toString());
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
            } else if (index < BASE_LENGTH + method.getMaxLocals()) {
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

class InterpreterFrame_Exec(var method: ResolvedJavaMethod, parent: InterpreterFrame, additionalStackSpace: Int) 
extends Frame_Exec(method.getMaxLocals() + method.getMaxStackSize() + InterpreterFrame.BASE_LENGTH + additionalStackSpace, parent) 
with InterpreterFrame {

    import Frame._
    import InterpreterFrame._

    var bci: Int = _
    var nextBci: Int = _
    var returnValue: Object = _ // TODO: map to stack

    assert(additionalStackSpace >= 0);

    setMethod(method);
    setBCI(0);
    setNextBCI(-1);
        

    /** Pointer to the top-most stack frame element. */
    private var tos: Int = BASE_LENGTH;

    def this(method: ResolvedJavaMethod, additionalStackSpace: Int) {
        this(method, null, additionalStackSpace);
    }

    def create(method: ResolvedJavaMethod, hasReceiver: Boolean, additionalStackSpace: Int, useParentArguments: Boolean): InterpreterFrame = {
        val frame = new InterpreterFrame_Exec(method, this, additionalStackSpace);

        if (useParentArguments) {
            val length = method.getSignature().getParameterSlots(hasReceiver);
            assert(length >= 0);

            frame.pushVoid(method.getMaxLocals());
            if (length > 0) {
                copyArguments(frame, length);
                popVoid(length);
            }
        }

        return frame;
    }

    def copy = this

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
        return BASE_LENGTH + getMethod().getMaxLocals();
    }

    private def copyArguments(dest: InterpreterFrame_Exec, length: Int): Unit = {
        System.arraycopy(locals, tosSingle(length - 1), dest.locals, BASE_LENGTH, length);
        //System.arraycopy(primitiveLocals, tosSingle(length - 1), dest.primitiveLocals, BASE_LENGTH, length);
    }


    def getReturnValue(): Object = {
      returnValue
    }
    def setReturnValueObject(value: Object): Unit = {
      returnValue = value
    }
    def setReturnValueInt(value: Int): Unit = {
      returnValue = value: java.lang.Integer
    }
    def setReturnValueLong(value: Long): Unit = {
      returnValue = value: java.lang.Long
    }
    def setReturnValueFloat(value: Float): Unit = {
      returnValue = value: java.lang.Float
    }
    def setReturnValueDouble(value: Double): Unit = {
      returnValue = value: java.lang.Double
    }

    def peekReceiver(method: ResolvedJavaMethod): Object = {
        return getObject(tosSingle(method.getSignature().getParameterSlots(false)));
    }

    /*def pushBoth(oValue: Object, intValue: Int): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), oValue);
        setInt(tosSingle(0), intValue);
    }

    def pushBoth(oValue: Object, longValue: Long): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), oValue);
        setLong(tosSingle(0), longValue);
    }*/

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
        //val tmpInt = getInt(tosSingle(1));
        val tmpObject = getObject(tosSingle(1));

        //setInt(tosSingle(1), getInt(tosSingle(0)));
        setObject(tosSingle(1), getObject(tosSingle(0)));

        //setInt(tosSingle(0), tmpInt);
        setObject(tosSingle(0), tmpObject);
    }

    def dupx1(): Unit = {
        //val tosLong = getLong(tosSingle(0));
        val tosObject = getObject(tosSingle(0));

        swapSingle();

        //pushBoth(tosObject, tosLong);
        pushObject(tosObject)
    }

    def dup2x1(): Unit = {
        //val tosLong2 = getLong(tosSingle(2));
        val tosObject2 = getObject(tosSingle(2));
        //val tosLong1 = getLong(tosSingle(1));
        val tosObject1 = getObject(tosSingle(1));
        //val tosLong0 = getLong(tosSingle(0));
        val tosObject0 = getObject(tosSingle(0));

        popVoid(3);

        //pushBoth(tosObject1, tosLong1);
        //pushBoth(tosObject0, tosLong0);
        pushObject(tosObject1)
        pushObject(tosObject0)

        //pushBoth(tosObject2, tosLong2);
        pushObject(tosObject2)

        //pushBoth(tosObject1, tosLong1);
        //pushBoth(tosObject0, tosLong0);
        pushObject(tosObject1)
        pushObject(tosObject0)
    }

    def dup2x2(): Unit = {
        //val tosLong3 = getLong(tosSingle(3));
        val tosObject3 = getObject(tosSingle(3));
        //val tosLong2 = getLong(tosSingle(2));
        val tosObject2 = getObject(tosSingle(2));
        //val tosLong1 = getLong(tosSingle(1));
        val tosObject1 = getObject(tosSingle(1));
        //val tosLong0 = getLong(tosSingle(0));
        val tosObject0 = getObject(tosSingle(0));

        popVoid(4);

        //pushBoth(tosObject1, tosLong1);
        //pushBoth(tosObject0, tosLong0);
        pushObject(tosObject1)
        pushObject(tosObject0)

        //pushBoth(tosObject3, tosLong3);
        //pushBoth(tosObject2, tosLong2);
        pushObject(tosObject3)
        pushObject(tosObject2)

        //pushBoth(tosObject1, tosLong1);
        //pushBoth(tosObject0, tosLong0);
        pushObject(tosObject1)
        pushObject(tosObject0)
    }

    def dupx2(): Unit = {
        //val tosLong2 = getLong(tosSingle(2));
        val tosObject2 = getObject(tosSingle(2));
        //val tosLong1 = getLong(tosSingle(1));
        val tosObject1 = getObject(tosSingle(1));
        //val tosLong0 = getLong(tosSingle(0));
        val tosObject0 = getObject(tosSingle(0));

        popVoid(3);

        //pushBoth(tosObject0, tosLong0);
        //pushBoth(tosObject2, tosLong2);
        //pushBoth(tosObject1, tosLong1);
        //pushBoth(tosObject0, tosLong0);
        pushObject(tosObject0)
        pushObject(tosObject2)
        pushObject(tosObject1)
        pushObject(tosObject0)
    }

    def dup(length: Int): Unit = {
        assert (length > 0);
        var i = 0
        while (i < length) {
            //val valueN1 = getLong(tosSingle(length - 1));
            val valueO1 = getObject(tosSingle(length - 1));

            pushVoid(1);

            //setLong(tosSingle(0), valueN1);
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

    def setStackTop(x: Int): Unit = { // unsafe!
        tos = x
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
        this.method = method
    }

    def getMethod(): ResolvedJavaMethod = {
        return method
    }

    def setBCI(bci: Int): Unit = {
        this.bci = bci
    }

    def getBCI(): Int = {
        return bci
    }

    def setNextBCI(bci: Int): Unit = {
        this.nextBci = bci
    }

    def getNextBCI(): Int = {
        return nextBci
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

    def setParentFrame(frame: InterpreterFrame): Unit = {
        setObject(PARENT_FRAME_SLOT, frame)
    }

    def dispose(): Unit = {
        // Clear out references in locals array.
        Arrays.fill(locals, null);
    }

    override def toString(): String = {
        val method = getMethod();
        val b = new StringBuilder(getMethod().asStackTraceElement(getBCI()).toString());
        for (i <- 0 until tos) {
            val obj = getObject(tosSingle(i));
            //val primitive = getLong(tosSingle(i));

            var objectString: String = null;
            if (obj != null) {
                objectString = obj.getClass().getSimpleName() + "@" + Integer.toHexString(obj.hashCode());
            }
            //val primitiveString = "0x" + java.lang.Long.toHexString(primitive).toUpperCase();
            val primitiveString = "(primitive?)"
            var typeString: String = null;

            val index = tosSingle(i);
            if (index == METHOD_FRAME_SLOT) {
                typeString = "method";
            } else if (index == BCI_FRAME_SLOT) {
                typeString = "bci";
            } else if (index == PARENT_FRAME_SLOT) {
                typeString = "parent";
            } else if (index < BASE_LENGTH + method.getMaxLocals()) {
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

