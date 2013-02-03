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
package lancet.interpreter

import lancet.core._

import java.lang.reflect.{Array=>jlrArray,_};
import java.util._;
import sun.misc._;

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;



trait InterpreterUniverse_Str extends RuntimeUniverse_Str with InterpreterUniverse {


object Frame {
    final val EMPTY_ARRAY = new Array[Object](0)
    final val PARENT_FRAME_SLOT = 0;
    final val MIN_FRAME_SIZE = 1;
}


// local array is staged

class Frame_Str0(val numLocals: Int, parent: Frame) extends Frame {
    import Frame._
    assert(numLocals >= MIN_FRAME_SIZE);

    var locals: Rep[Array[Object]] = _//reflect("new Array[Object]("+numLocals+")")
    var primitiveLocals: Rep[Array[Long]] = _//reflect("new Array[Long]("+numLocals+")")

    //reflect("locals(PARENT_FRAME_SLOT) = parent")

    def this(numLocals: Int) = this(numLocals, null);

    def getObject(index: Int): Rep[Object] = {
        reflect[Object](locals+"("+index+")")
    }

    def setObject(index: Int, value: Rep[Object]): Unit = {
        reflect[Unit](locals+"("+index+") = "+value)
    }

    def getFloat(index: Int): Rep[Float] = {
        return unsafe.getFloat(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setFloat(index: Int, value: Rep[Float]): Unit = {
        unsafe.putFloat(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getLong(index: Int): Rep[Long] = {
        return unsafe.getLong(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setLong(index: Int, value: Rep[Long]): Unit = {
        unsafe.putLong(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getInt(index: Int): Rep[Int] = {
        return unsafe.getInt(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setInt(index: Int, value: Rep[Int]): Unit = {
        unsafe.putInt(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getDouble(index: Int): Rep[Double] = {
        return unsafe.getDouble(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setDouble(index: Int, value: Rep[Double]): Unit = {
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

    def getArguments(argOffset: Int): Rep[Array[Object]] = {
        return getObject(argOffset).asInstanceOf[Rep[Array[Object]]]; // TODO: dynamic cast
    }

    def size: Int = {
        return numLocals//locals.length;
    }
    
}


// local array contains staged values

class Frame_Str(val numLocals: Int, val parent: Frame) extends Frame {
    import Frame._
    assert(numLocals >= MIN_FRAME_SIZE);

    val locals: Array[Rep[Object]] = {
        implicit val m = repManifest[Object]
        new Array[Rep[Object]](numLocals)
    }

    //reflect("locals(PARENT_FRAME_SLOT) = parent")

    def this(numLocals: Int) = this(numLocals, null);

    def getObject(index: Int): Rep[Object] = {
        locals(index)
    }

    def setObject(index: Int, value: Rep[Object]): Unit = {
        locals(index) = value
    }

    def getFloat(index: Int): Rep[Float] = {
        locals(index).asInstanceOf[Rep[Float]]
    }

    def setFloat(index: Int, value: Rep[Float]): Unit = {
        locals(index) = value.asInstanceOf[Rep[Object]]
    }

    def getLong(index: Int): Rep[Long] = {
        locals(index).asInstanceOf[Rep[Long]]
    }

    def setLong(index: Int, value: Rep[Long]): Unit = {
        locals(index) = value.asInstanceOf[Rep[Object]]
    }

    def getInt(index: Int): Rep[Int] = {
        locals(index).asInstanceOf[Rep[Int]]
    }

    def setInt(index: Int, value: Rep[Int]): Unit = {
        locals(index) = value.asInstanceOf[Rep[Object]]
    }

    def getDouble(index: Int): Rep[Double] = {
        locals(index).asInstanceOf[Rep[Double]]
    }

    def setDouble(index: Int, value: Rep[Double]): Unit = {
        locals(index) = value.asInstanceOf[Rep[Object]]
    }

    def getParentFrame(level: Int): Frame = {
        assert(level >= 0);
        if (level == 0) {
            return this;
        } else {
            return parent.getParentFrame(level - 1);
        }
    }

    def getTopFrame(): Frame = {
        val parentFrame = parent
        if (parentFrame == null) {
            return this;
        } else {
            return parentFrame.getTopFrame();
        }
    }

    def getArguments(argOffset: Int): Rep[Array[Object]] = {
        return getObject(argOffset).asInstanceOf[Rep[Array[Object]]]; // TODO: dynamic cast
    }

    def size: Int = {
        return numLocals//locals.length;
    }
    
}




object InterpreterFrame {
    final val BASE_LENGTH = 3;

    final val METHOD_FRAME_SLOT = 1;
    final val BCI_FRAME_SLOT = 2;

    final val DOUBLE = 2;
    final val SINGLE = 1;
}


class InterpreterFrame_Str(var method: ResolvedJavaMethod, parent: InterpreterFrame, additionalStackSpace: Int) 
extends Frame_Str(method.maxLocals() + method.maxStackSize() + InterpreterFrame.BASE_LENGTH + additionalStackSpace, parent) 
with InterpreterFrame {

    import Frame._
    import InterpreterFrame._

    var bci: Int = _

    var returnValue: Rep[Object] = _

    assert(additionalStackSpace >= 0);

    setMethod(method);
    setBCI(0);
        

    /** Pointer to the top-most stack frame element. */
    private var tos: Int = BASE_LENGTH;

    def this(method: ResolvedJavaMethod, additionalStackSpace: Int) {
        this(method, null, additionalStackSpace);
    }

    def create(method: ResolvedJavaMethod, hasReceiver: Boolean, additionalStackSpace: Int, useParentArguments: Boolean): InterpreterFrame = {
        val frame = new InterpreterFrame_Str(method, this, additionalStackSpace);

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

    def copy() = {
      //assert(parent != null, "don't copy root frame!")
      val frame = new InterpreterFrame_Str(method, parent, additionalStackSpace);
      System.arraycopy(locals, 0, frame.locals, 0, locals.length)
      //frame.locals = locals
      //frame.primitiveLocals = primitiveLocals
      frame.returnValue = returnValue
      frame.bci = bci
      frame.tos = tos
      frame
    }

    def copy2(parent: InterpreterFrame_Str) = { // TODO
      //assert(parent != null, "don't copy root frame!")
      val frame = new InterpreterFrame_Str(method, parent, additionalStackSpace);
      System.arraycopy(locals, 0, frame.locals, 0, locals.length)
      //frame.locals = locals
      //frame.primitiveLocals = primitiveLocals
      frame.returnValue = returnValue
      frame.bci = bci
      frame.tos = tos
      frame
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

    private def copyArguments(dest: InterpreterFrame_Str, length: Int): Unit = {
        System.arraycopy(locals, tosSingle(length - 1), dest.locals, BASE_LENGTH, length)
    }


    def getReturnValue(): Rep[Object] = {
      returnValue
    }
    def setReturnValueObject(value: Rep[Object]): Unit = {
      returnValue = value
    }
    def setReturnValueInt(value: Rep[Int]): Unit = {
      returnValue = value.asInstanceOf[Rep[Object]]
    }
    def setReturnValueLong(value: Rep[Long]): Unit = {
      returnValue = value.asInstanceOf[Rep[Object]]
    }
    def setReturnValueFloat(value: Rep[Float]): Unit = {
      returnValue = value.asInstanceOf[Rep[Object]]
    }
    def setReturnValueDouble(value: Rep[Double]): Unit = {
      returnValue = value.asInstanceOf[Rep[Object]]
    }

    def peekReceiver(method: ResolvedJavaMethod): Rep[Object] = {
        return getObject(tosSingle(method.signature().argumentSlots(false)));
    }

    def pushBoth(oValue: Rep[Object], intValue: Rep[Int]): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), oValue);
        setInt(tosSingle(0), intValue);
    }

    class OVHack
    implicit val ovhack = new OVHack

    def pushBoth(oValue: Rep[Object], longValue: Rep[Long])(implicit e: OVHack): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), oValue);
        setLong(tosSingle(0), longValue);
    }

    def pushObject(value: Rep[Object]): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), value);
    }

    def pushBoolean(value: Rep[Boolean]): Unit = {
        pushInt(if_ (value) (1) (0));
    }

    def pushByte(value: Rep[Byte]): Unit = {
        pushInt(value);
    }

    def pushShort(value: Rep[Short]): Unit = {
        pushInt(value);
    }

    def pushChar(value: Rep[Char]): Unit = {
        pushInt(value);
    }

    def pushInt(value: Rep[Int]): Unit = {
        incrementTos(SINGLE);
        setInt(tosSingle(0), value);
    }

    def pushDouble(value: Rep[Double]): Unit = {
        incrementTos(DOUBLE);
        setDouble(tosDouble(0), value);
    }

    def pushFloat(value: Rep[Float]): Unit = {
        incrementTos(SINGLE);
        setFloat(tosSingle(0), value);
    }

    def pushLong(value: Rep[Long]): Unit = {
        incrementTos(DOUBLE);
        setLong(tosDouble(0), value);
    }

    def popBoolean(): Rep[Boolean] = {
        val value = popInt();
        assert(value == 0 || value == 1);
        return value == 1;
    }

    def popByte(): Rep[Byte] = {
        val value = popInt();
        //assert (value >= Byte.MinValue && value <= Byte.MaxValue);
        return value.toByte;
    }

    def popChar(): Rep[Char] = {
        val value = popInt();
        //assert (value >= Char.MinValue && value <= Char.MaxValue);
        return value.toChar;
    }

    def popShort(): Rep[Short] = {
        val value = popInt();
        //assert (value >= Short.MinValue && value <= Short.MaxValue);
        return value.toShort;
    }

    def popInt(): Rep[Int] = {
        val value = getInt(tosSingle(0));
        decrementTos(SINGLE);
        return value;
    }

    def popDouble(): Rep[Double] = {
        val value = getDouble(tosDouble(0));
        decrementTos(DOUBLE);
        return value;
    }

    def popFloat(): Rep[Float] = {
        val value = getFloat(tosSingle(0));
        decrementTos(SINGLE);
        return value;
    }

    def popLong(): Rep[Long] = {
        val value = getLong(tosDouble(0));
        decrementTos(DOUBLE);
        return value;
    }

    def popObject(): Rep[Object] = {
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
        // TR: null slots!
        for (i <- 0 until size) {
            locals(tos+i) = null.asInstanceOf[Rep[Object]]
        }
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

    /*def pushTo(childFrame: InterpreterFrame, argumentSlots: Int): Unit = {
        System.arraycopy(locals, tos - argumentSlots, childFrame.locals,
                        Frame.MIN_FRAME_SIZE, argumentSlots);

        System.arraycopy(primitiveLocals, tos - argumentSlots, childFrame.primitiveLocals,
                        Frame.MIN_FRAME_SIZE, argumentSlots);
        popVoid(argumentSlots);
    }*/

    def getParentFrame(): InterpreterFrame = {
        //return getObject(PARENT_FRAME_SLOT).asInstanceOf[InterpreterFrame];
        //throw new Exception("not supported")
        parent.asInstanceOf[InterpreterFrame]
    }

    def dispose(): Unit = {
        // Clear out references in locals array.
        Arrays.fill(locals.asInstanceOf[Array[Object]], null)
        returnValue = null.asInstanceOf[Rep[Object]]
    }

/*    override def toString(): String = {
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
*/
    def popStack(): Unit = {
        // TODO(chumer): prevent popping local variables.
        popVoid(tos - stackTos());
    }

}

}
