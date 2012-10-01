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




// bytecode interpreter

/**
 * High-level bytecode interpreter that executes on top of Java. Java native methods
 * are executed using the {@link com.oracle.graal.api.interpreter.RuntimeInterpreterInterface}.
 */


trait BytecodeInterpreter extends InterpreterUniverse {
  

trait InterpreterCallable {
    // methods
    def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, arguments: Array[Rep[Object]]): Rep[Object] // throws Throwable;
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






  def addDelegate(method: Method, callable: InterpreterCallable): Unit
}

object BytecodeInterpreter {
    final val OPTION_MAX_STACK_SIZE = "maxStackSize";
    final val TRACE = true;
    final val TRACE_BYTE_CODE = true;

    final val DEFAULT_MAX_STACK_SIZE = 1500;

    final val NEXT = -1;
    final val BRANCH = -2;
    final val BRANCH_COND = -3;
    final val RETURN = -4;
    final val CALL = -5;
}



trait BytecodeInterpreter_Abstract extends BytecodeInterpreter { self =>

  import BytecodeInterpreter._

  def runtimeInterface: Runtime
  def metaAccessProvider: MetaAccessProvider
  def maxStackFrames: Int


  protected class MethodRedirectionInfo(val receiver: InterpreterCallable)
  def methodDelegates: Map[ResolvedJavaMethod, MethodRedirectionInfo];

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


  def traceOp(frame: InterpreterFrame, opName: String): Unit = {
    trace(frame.depth(), opName);
  }

  def traceCall(frame: InterpreterFrame, typ: String) {
    trace(frame.depth(), typ + " " + frame.getMethod() + " - " + frame.getMethod().signature().asString());
  }

  def trace(level: Int, message: String) {
    val builder = new StringBuilder();
    var i = 0
    while (i < level) {
        builder.append("  ");
        i += 1
    }
    builder.append(message);
    System.out.println(builder);
  }


  var callFrame: InterpreterFrame = null

  type Control = (InterpreterFrame, BytecodeStream) => Any

  def branch(): Control = { (frame, bs) =>
    executeBlock(frame, bs, bs.readBranchDest())
  }

  def conditionalBranch(c: Rep[Boolean]): Control = { (frame, bs) =>
    val a = bs.readBranchDest()
    val b = bs.nextBCI()
    // need to retain stack !!!
    val tos = frame.stackTos
    if_ (c) {
      assert(tos ==frame.stackTos, "illegal stack change (then branch)")
      executeBlock(frame, bs, a)
    }{
      assert(tos ==frame.stackTos, "illegal stack change (else branch)")
      executeBlock(frame, bs, b)
    }
  }


  def call(callFrame: InterpreterFrame): Control = { (frame, bs) =>
    if (callFrame != null) {
      this.callFrame = callFrame
      allocateFrame(frame, bs)
      val method = callFrame.getMethod()
      val bs1 = new BytecodeStream(method.code())
      executeBlock(callFrame, bs1, callFrame.getBCI());
      val returnValue = callFrame.getReturnValue()
      pushAsObjectInternal(frame, method.signature().returnKind(), returnValue);
    } else {      
      // trampoline executeBlock instead of falling through?
    }
    executeBlock(frame, bs, bs.nextBCI())
  }

  def retn(): Control = { (frame, bs) =>

    popFrame(frame)
  }

  def thrw(t: Rep[Throwable]): Control = { (frame, bs) =>

    popFrame(frame) // FIXME
  }



  /*def executeBlocks(frame: Interpreter, bs: BytecodeStream, bci: Int): Unit = {
    var ctrl: Control = executeBlock(frame, bs, bci)
    do {

    }
  }*/

  def executeBlock(frame: InterpreterFrame, bs: BytecodeStream, bci: Int): Rep[Unit] = {
    //println("-- exec block bci "+bci)
    bs.setBCI(bci)
    var ctrl: Control = null
    do {
      ctrl = executeInstruction(frame, bs)
      //println("--- ctrl " + ctrl)
      if (ctrl == null)
        bs.next()
    } while (ctrl == null)
    val r = ctrl(frame, bs).asInstanceOf[Rep[Unit]]
    //println("--- res " + r.getClass)
    //bs.next()
    r
  }




  def executeInstruction(frame: InterpreterFrame, bs: BytecodeStream): Control = {// throws Throwable {
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
          return conditionalBranch(frame.popInt() == 0)
      case Bytecodes.IFNE =>
          return conditionalBranch(frame.popInt() != 0)
      case Bytecodes.IFLT =>
          return conditionalBranch(frame.popInt() < 0)
      case Bytecodes.IFGE =>
          return conditionalBranch(frame.popInt() >= 0)
      case Bytecodes.IFGT =>
          return conditionalBranch(frame.popInt() > 0)
      case Bytecodes.IFLE =>
          return conditionalBranch(frame.popInt() <= 0)
      case Bytecodes.IF_ICMPEQ =>
          return conditionalBranch(frame.popInt() == frame.popInt())
      case Bytecodes.IF_ICMPNE =>
          return conditionalBranch(frame.popInt() != frame.popInt())
      case Bytecodes.IF_ICMPLT =>
          return conditionalBranch(frame.popInt() > frame.popInt())
      case Bytecodes.IF_ICMPGE =>
          return conditionalBranch(frame.popInt() <= frame.popInt())
      case Bytecodes.IF_ICMPGT =>
          return conditionalBranch(frame.popInt() < frame.popInt())
      case Bytecodes.IF_ICMPLE =>
          return conditionalBranch(frame.popInt() >= frame.popInt())
      case Bytecodes.IF_ACMPEQ =>
          return conditionalBranch(frame.popObject() == frame.popObject())
      case Bytecodes.IF_ACMPNE =>
          return conditionalBranch(frame.popObject() != frame.popObject())
      case Bytecodes.GOTO | Bytecodes.GOTO_W =>
          return branch();
      case Bytecodes.JSR | Bytecodes.JSR_W =>
          frame.pushInt(bs.currentBCI());
          return branch();
      case Bytecodes.RET =>
          assert(false, "RET not implemented")
          //bs.setBCI(frame.getInt(frame.resolveLocalIndex(bs.readLocalIndex())));
          return null // TR should return BRANCH??
      case Bytecodes.TABLESWITCH =>
          return tableSwitch _ //(frame, bs);
      case Bytecodes.LOOKUPSWITCH =>
          return lookupSwitch _ //(frame, bs);
      case Bytecodes.IRETURN =>
          //frame.getParentFrame().pushInt(frame.popInt());
          frame.setReturnValueInt(frame.popInt())
          return retn();
      case Bytecodes.LRETURN =>
          //frame.getParentFrame().pushLong(frame.popLong());
          frame.setReturnValueLong(frame.popLong())
          return retn();
      case Bytecodes.FRETURN =>
          //frame.getParentFrame().pushFloat(frame.popFloat());
          frame.setReturnValueFloat(frame.popFloat())
          return retn();
      case Bytecodes.DRETURN =>
          //frame.getParentFrame().pushDouble(frame.popDouble());
          frame.setReturnValueDouble(frame.popDouble())
          return retn();
      case Bytecodes.ARETURN =>
          //frame.getParentFrame().pushObject(frame.popObject());
          frame.setReturnValueObject(frame.popObject())
          return retn();
      case Bytecodes.RETURN =>
          return retn();
      case Bytecodes.GETSTATIC =>
          getField(frame, null, bs.currentBC(), bs.readCPI());
      case Bytecodes.PUTSTATIC =>
          putStatic(frame, bs.readCPI());
      case Bytecodes.GETFIELD =>
          getField(frame, nullCheck(frame.popObject()), bs.currentBC(), bs.readCPI());
      case Bytecodes.PUTFIELD =>
          putField(frame, bs.readCPI());
      case Bytecodes.INVOKEVIRTUAL =>
          return call(invokeVirtual(frame, bs.readCPI()))
      case Bytecodes.INVOKESPECIAL =>
          return call(invokeSpecial(frame, bs.readCPI()));
      case Bytecodes.INVOKESTATIC =>
          return call(invokeStatic(frame, bs.readCPI()));
      case Bytecodes.INVOKEINTERFACE =>
          return call(invokeInterface(frame, bs.readCPI()));
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
          frame.pushInt(runtimeInterface.arrayLength(nullCheck(frame.popObject())));
      case Bytecodes.ATHROW =>
          val t = frame.popObject().asInstanceOf[Rep[Throwable]];
          return thrw(t);
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
          return conditionalBranch(frame.popObject() == null)
      case Bytecodes.IFNONNULL =>
          return conditionalBranch(frame.popObject() != null)
      case Bytecodes.BREAKPOINT =>
          assert(false, "no breakpoints supported at this time.");
    }
    return null
  }

    def allocateFrame(frame: InterpreterFrame, bs: BytecodeStream): InterpreterFrame = {
        try {
            val nextFrame: InterpreterFrame = this.callFrame;

            assert(nextFrame != null);
            //TRassert(nextFrame.getParentFrame() == frame);

            // store bci when leaving method
            //TRframe.setBCI(bs.currentBCI());
            frame.setBCI(bs.nextBCI());

            if (TRACE) {
                traceCall(nextFrame, "Call");
            }
            if (Modifier.isSynchronized(nextFrame.getMethod().accessFlags())) {
                if (TRACE) {
                    traceOp(frame, "Method monitor enter");
                }
                if (Modifier.isStatic(nextFrame.getMethod().accessFlags())) {
                    runtimeInterface.monitorEnter(unit(nextFrame.getMethod().holder().toJava()));
                } else {
                    val enterObject = nextFrame.getObject(frame.resolveLocalIndex(0));
                    //assert(enterObject =!= null);
                    runtimeInterface.monitorEnter(enterObject);
                }
            }

            return nextFrame;
        } finally {
            callFrame = null;
            //bs.next();
        }
    }

    def popFrame(frame: InterpreterFrame): InterpreterFrame = {
        //val parent: InterpreterFrame = frame.getParentFrame();
        if (Modifier.isSynchronized(frame.getMethod().accessFlags())) {
            if (TRACE) {
                traceOp(frame, "Method monitor exit");
            }
            if (Modifier.isStatic(frame.getMethod().accessFlags())) {
                runtimeInterface.monitorExit(unit(frame.getMethod().holder().toJava()));
            } else {
                val exitObject = frame.getObject(frame.resolveLocalIndex(0));
                //if (exitObject =!= null) {
                    runtimeInterface.monitorExit(exitObject);
                //}
            }
        }
        if (TRACE) {
            traceCall(frame, "Ret");
        }

        frame.dispose();
        return null//parent;
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
        return lookupSearch(bs, frame.popInt());
    }

    private def tableSwitch(frame: InterpreterFrame, bs: BytecodeStream): Int = {
        return tableSearch(bs, frame.popInt())
    }

    protected def lookupSearch(bs: BytecodeStream, key: Rep[Int]): Int /*= {
        val switchHelper = new BytecodeLookupSwitch(bs, bs.currentBCI())

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
    }*/

    protected def tableSearch(bs: BytecodeStream, index: Rep[Int]): Int /*= {
        val switchHelper = new BytecodeTableSwitch(bs, bs.currentBCI());

        val low = switchHelper.lowKey();
        val high = switchHelper.highKey();

        assert(low <= high);

        if (index < low || index > high) {
            return switchHelper.defaultTarget();
        } else {
            return switchHelper.targetAt(index - low);
        }
    }*/

    protected def nullCheck(value: Rep[Object]): Rep[Object] = {
        runtimeInterface.nullCheck(value)
    }

    protected def checkCastInternal(typ: ResolvedJavaType, value: Rep[Object]): Rep[Object] /*=
      reflect("checkCast("+typ.toJava+","+value+")")*/

    protected def checkCast(frame: InterpreterFrame, cpi: Char): Unit = {
        val typ = resolveType(frame, Bytecodes.CHECKCAST, cpi)
        frame.pushObject(checkCastInternal(typ,frame.popObject()));
    }

    protected def instanceOf(frame: InterpreterFrame, cpi: Char): Unit = {
        frame.pushInt(if_ (runtimeInterface.typeIsInstance(resolveType(frame, Bytecodes.INSTANCEOF, cpi), frame.popObject())) (1) (0));
    }

    protected def resolveType(frame: InterpreterFrame, opcode: Int, cpi: Char): ResolvedJavaType = {
        val constantPool: ConstantPool = frame.getConstantPool();
        constantPool.loadReferencedType(cpi, opcode);
        return constantPool.lookupType(cpi, opcode).resolve(frame.getMethod().holder());
    }

    protected def resolveType(frame: InterpreterFrame, javaClass: Class[_]): ResolvedJavaType = {
        return metaAccessProvider.getResolvedJavaType(javaClass).resolve(frame.getMethod().holder());
    }

    protected def resolveMethod(frame: InterpreterFrame, opcode: Int, cpi: Char): ResolvedJavaMethod = {
        val constantPool: ConstantPool = frame.getConstantPool();
        constantPool.loadReferencedType(cpi, opcode);
        return constantPool.lookupMethod(cpi, opcode).asInstanceOf[ResolvedJavaMethod];
    }

    protected def resolveField(frame: InterpreterFrame, opcode: Int, cpi: Char): ResolvedJavaField = {
        val constantPool: ConstantPool = frame.getConstantPool();
        constantPool.loadReferencedType(cpi, opcode);
        return constantPool.lookupField(cpi, opcode).asInstanceOf[ResolvedJavaField];
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
                    frame.pushObject(unit(c.asObject()));
                case Kind.Double =>
                    frame.pushDouble(c.asDouble());
                case Kind.Long =>
                    frame.pushLong(c.asLong());
                case _ =>
                    assert(false, "unspecified case")
            }
        } else if (constant.isInstanceOf[JavaType]) {
            frame.pushObject(unit((constant.asInstanceOf[JavaType]).resolve(method.holder()).toJava()));
        } else {
            assert(false, "unexpected case");
        }
    }

    private def compareLong(frame: InterpreterFrame) {
        val y = frame.popLong();
        val x = frame.popLong();
        frame.pushInt(if_ (x < y) (-1) (if_ (x === y) (0) (1)));
    }

    private def compareDoubleGreater(frame: InterpreterFrame) {
        val y = frame.popDouble();
        val x = frame.popDouble();
        frame.pushInt(if_ (x < y) (-1) (if_ (x === y) (0) (1)));
    }

    private def compareDoubleLess(frame: InterpreterFrame) {
        val y = frame.popDouble();
        val x = frame.popDouble();
        frame.pushInt(if_ (x > y) (1) (if_ (x === y) (0) (-1)));
    }

    private def compareFloatGreater(frame: InterpreterFrame) {
        val y = frame.popFloat();
        val x = frame.popFloat();
        frame.pushInt(if_ (x < y) (-1) (if_ (x === y) (0) (1)));
    }

    private def compareFloatLess(frame: InterpreterFrame) {
        val y = frame.popFloat();
        val x = frame.popFloat();
        frame.pushInt(if_ (x > y) (1) (if_ (x === y) (0) (-1)));
    }

    private def invokeStatic(frame: InterpreterFrame, cpi: Char): InterpreterFrame = {// throws Throwable {
        return invoke(frame, resolveMethod(frame, Bytecodes.INVOKESTATIC, cpi), null, false);
    }

    private def invokeInterface(frame: InterpreterFrame, cpi: Char): InterpreterFrame = {// throws Throwable {
        return resolveAndInvoke(frame, resolveMethod(frame, Bytecodes.INVOKEINTERFACE, cpi));
    }

    def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame /*= {// throws Throwable {
        val receiver: Object = nullCheck(parent.peekReceiver(m));

        val method: ResolvedJavaMethod = resolveType(parent, receiver.getClass()).resolveMethodImpl(m);

        if (method == null) {
            throw new AbstractMethodError();
        }

        return invoke(parent, method, receiver);
    }*/

    private def invokeVirtual(frame: InterpreterFrame, cpi: Char): InterpreterFrame = {// throws Throwable {
        val m: ResolvedJavaMethod = resolveMethod(frame, Bytecodes.INVOKEVIRTUAL, cpi);
        if (Modifier.isFinal(m.accessFlags())) {
            return invoke(frame, m, nullCheck(frame.peekReceiver(m)), true);
        } else {
            return resolveAndInvoke(frame, m);
        }
    }

    private def invokeSpecial(frame: InterpreterFrame, cpi: Char): InterpreterFrame = {// throws Throwable {
        val m: ResolvedJavaMethod = resolveMethod(frame, Bytecodes.INVOKESPECIAL, cpi);
        return invoke(frame, m, nullCheck(frame.peekReceiver(m)), true);
    }


    def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, receiver: Rep[Object], hasReceiver: Boolean): InterpreterFrame = {// throws Throwable {
        if (caller.depth() >= maxStackFrames) {
            throw new StackOverflowError("Maximum callstack of " + maxStackFrames + " exceeded.");
        }

        val redirectedMethod: MethodRedirectionInfo = methodDelegates.get(method);
        if (redirectedMethod != null) {
            return invokeRedirectedMethod(caller, method, redirectedMethod, hasReceiver);
        } else {
          if (Modifier.isNative(method.accessFlags())) {
            return invokeNativeMethod(caller, method, hasReceiver);
          } else {
            return invokeDirect(caller, method, hasReceiver);
          }
        }
    }

    private def invokeNativeMethod(caller: InterpreterFrame, method: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame = {// throws Throwable {
        assert(!methodDelegates.containsKey(method), "must not be redirected");
        if (TRACE) {
            traceCall(caller, "Native " + method);
        }

        // mark the current thread as high level and execute the native method
        val parameters = popArgumentsAsObject(caller, method, hasReceiver);
        val returnValue = runtimeInterface.invoke(method, parameters);
        pushAsObject(caller, method.signature().returnKind(), returnValue);

        return null;
    }

    private def invokeRedirectedMethod(caller: InterpreterFrame, originalMethod: ResolvedJavaMethod, redirectionInfo: MethodRedirectionInfo, hasReceiver: Boolean): InterpreterFrame = {// throws Throwable {
        assert(methodDelegates.containsKey(originalMethod), "must be redirected");
        if (TRACE) {
            traceCall(caller, "Delegate " + originalMethod);
        }

        // current thread is low level and we also execute the target method in the low-level interpreter
        val originalCalleeParameters = popArgumentsAsObject(caller, originalMethod, hasReceiver);
        val returnValue: Rep[Object] = redirectionInfo.receiver.invoke(caller, originalMethod, originalCalleeParameters);
        pushAsObject(caller, originalMethod.signature().returnKind(), returnValue);

        return null;
    }

    def invokeDirect(parent: InterpreterFrame, method: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame /*= {// throws Throwable {
        //return parent.create(method, hasReceiver);
        return parent.create(method, hasReceiver, 0, true);
    }*/

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

    def popArgumentsAsObject(frame: InterpreterFrame, method: ResolvedJavaMethod, hasReceiver: Boolean): Array[Rep[Object]] = {
        implicit val mf: Manifest[Rep[Object]] = repManifest[Object]

        val signature: Signature = method.signature();
        val argumentCount: Int = method.signature().argumentCount(hasReceiver);
        val parameters = new Array[Rep[Object]](argumentCount);

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


    def allocateMultiArray(frame: InterpreterFrame, cpi: Char, dimension: Int): Rep[Object] = {
        val typ: ResolvedJavaType = getLastDimensionType(resolveType(frame, Bytecodes.MULTIANEWARRAY, cpi));

        implicit val mf: Manifest[Rep[Int]] = repManifest[Int]

        val dimensions = new Array[Rep[Int]](dimension);
        var i = dimension - 1
        while (i >= 0) {        
            dimensions(i) = frame.popInt();
            i -= 1
        }
        return runtimeInterface.newMultiArray(typ, dimensions);
    }

    def getLastDimensionType(typ: ResolvedJavaType): ResolvedJavaType = {
        var result: ResolvedJavaType = typ;
        while (result.isArrayClass()) {
            result = result.componentType();
        }
        return result;
    }

    def allocateArray(frame: InterpreterFrame, cpi: Char): Rep[Object] = {
        val typ: ResolvedJavaType = resolveType(frame, Bytecodes.ANEWARRAY, cpi);
        return runtimeInterface.newArray(typ, frame.popInt)
    }

    def allocateNativeArray(frame: InterpreterFrame, cpi: Byte): Rep[Object] = {
        // the constants for the cpi are loosely defined and no real cpi indices.
        cpi match {
            case 4 =>
                return runtimeInterface.newArray(classOf[Boolean], frame.popInt());
            case 5 =>
                return runtimeInterface.newArray(classOf[Char], frame.popInt());
            case 6 =>
                return runtimeInterface.newArray(classOf[Float], frame.popInt());
            case 7 =>
                return runtimeInterface.newArray(classOf[Double], frame.popInt());
            case 8 =>
                return runtimeInterface.newArray(classOf[Byte], frame.popInt());
            case 9 =>
                return runtimeInterface.newArray(classOf[Short], frame.popInt());
            case 10 =>
                return runtimeInterface.newArray(classOf[Int], frame.popInt());
            case 11 =>
                return runtimeInterface.newArray(classOf[Long], frame.popInt());
            case _ =>
                assert(false, "unexpected case");
                return null;
        }
    }

    def allocateInstance(frame: InterpreterFrame, cpi: Char): Rep[Object] = {// throws InstantiationException {
        return runtimeInterface.newObject(resolveType(frame, Bytecodes.NEW, cpi));
    }

    def iinc(frame: InterpreterFrame, bs: BytecodeStream): Unit = {
        val index: Int = frame.resolveLocalIndex(bs.readLocalIndex());
        frame.setInt(index, frame.getInt(index) + bs.readIncrement());
    }

    def putStatic(frame: InterpreterFrame, cpi: Char): Unit = {
        putFieldStatic(frame, resolveField(frame, Bytecodes.PUTSTATIC, cpi));
    }

    def putField(frame: InterpreterFrame, cpi: Char): Unit = {
        putFieldVirtual(frame, resolveField(frame, Bytecodes.PUTFIELD, cpi));
    }

    def putFieldStatic(frame: InterpreterFrame, field: ResolvedJavaField): Unit = {
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

    def putFieldVirtual(frame: InterpreterFrame, field: ResolvedJavaField): Unit = {
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

    def getField(frame: InterpreterFrame, base: Rep[Object], opcode: Int, cpi: Char): Unit = {
        val field: ResolvedJavaField = resolveField(frame, opcode, cpi);
        field.kind() match {
            case Kind.Boolean =>
                frame.pushInt(if_ (runtimeInterface.getFieldBoolean(base, field)) (1) (0));
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

    def pushAsObjectInternal(frame: InterpreterFrame, typeKind: Kind, value: Rep[Object]): Int = {
        typeKind match {
            case Kind.Int =>
                frame.pushInt(value.asInstanceOf[Rep[Int]]);
            case Kind.Long =>
                frame.pushLong(value.asInstanceOf[Rep[Long]]);
                return 2;
            case Kind.Boolean =>
                frame.pushInt(value.asInstanceOf[Rep[Int]]);
            case Kind.Byte =>
                frame.pushInt(value.asInstanceOf[Rep[Int]]);
            case Kind.Char =>
                frame.pushInt(value.asInstanceOf[Rep[Int]]);
            case Kind.Double =>
                frame.pushDouble(value.asInstanceOf[Rep[Double]]);
                return 2;
            case Kind.Float =>
                frame.pushFloat(value.asInstanceOf[Rep[Float]]);
            case Kind.Short =>
                frame.pushInt(value.asInstanceOf[Rep[Int]]);
            case Kind.Object =>
                frame.pushObject(value);
            case Kind.Void =>
                return 0;
            case _ =>
                assert(false, "case not specified");
        }
        return 1;
    }

    def pushAsObject(frame: InterpreterFrame, typeKind: Kind, value: Rep[Object]): Int = {
        typeKind match {
            case Kind.Int =>
                frame.pushInt(value.asInstanceOf[Rep[Int]]);
            case Kind.Long =>
                frame.pushLong(value.asInstanceOf[Rep[Long]]);
                return 2;
            case Kind.Boolean =>
                frame.pushInt(if_ (value.asInstanceOf[Rep[Boolean]]) (1) (0));
            case Kind.Byte =>
                frame.pushInt(value.asInstanceOf[Rep[Byte]]);
            case Kind.Char =>
                frame.pushInt(value.asInstanceOf[Rep[Char]]);
            case Kind.Double =>
                frame.pushDouble(value.asInstanceOf[Rep[Double]]);
                return 2;
            case Kind.Float =>
                frame.pushFloat(value.asInstanceOf[Rep[Float]]);
            case Kind.Short =>
                frame.pushInt(value.asInstanceOf[Rep[Short]]);
            case Kind.Object =>
                frame.pushObject(value);
            case Kind.Void =>
                return 0;
            case _ =>
                assert(false, "case not specified");
        }
        return 1;
    }

    def popAsObject(frame: InterpreterFrame, typeKind: Kind): Rep[Object] = {
        typeKind match {
            case Kind.Boolean =>
                return (frame.popInt() === 1).asInstanceOf[Rep[java.lang.Boolean]]
            case Kind.Byte =>
                return frame.popInt().toByte.asInstanceOf[Rep[java.lang.Byte]]
            case Kind.Char =>
                return frame.popInt().toChar.asInstanceOf[Rep[java.lang.Character]]
            case Kind.Double =>
                return frame.popDouble().asInstanceOf[Rep[java.lang.Double]]
            case Kind.Int =>
                return frame.popInt().asInstanceOf[Rep[java.lang.Integer]]
            case Kind.Float =>
                return frame.popFloat().asInstanceOf[Rep[java.lang.Float]]
            case Kind.Long =>
                return frame.popLong().asInstanceOf[Rep[java.lang.Long]]
            case Kind.Short =>
                return frame.popInt().toShort.asInstanceOf[Rep[java.lang.Short]]
            case Kind.Object =>
                return frame.popObject();
            case Kind.Void =>
                return null;
            case _ =>
                assert(false, "unexpected case")
        }
        return null;
    }

}


trait BytecodeInterpreter_Common extends BytecodeInterpreter_Abstract {

    import BytecodeInterpreter._

    var methodDelegates: Map[ResolvedJavaMethod, MethodRedirectionInfo] = _;

    var maxStackFrames: Int = DEFAULT_MAX_STACK_SIZE;

    var rootMethod: ResolvedJavaMethod = _;
    var runtimeInterface: Runtime = _;
    var metaAccessProvider: MetaAccessProvider = _;

    def getRuntimeInterface(m: MetaAccessProvider): Runtime

    def initialize(args: String): Boolean = {
        methodDelegates = new HashMap

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
        this.runtimeInterface = getRuntimeInterface(metaAccessProvider)

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
            def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, arguments: Array[Rep[Object]]): Rep[Object] = {// throws Throwable {
                setBackTrace(caller, arguments(0).asInstanceOf[Rep[Throwable]], createStackTraceElements(caller));
                return null;
            }
        });
        addDelegate(findMethod(classOf[Throwable], "getStackTraceDepth"), new InterpreterCallable() {

            //@Override
            def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, arguments: Array[Rep[Object]]): Rep[Object] = {// throws Throwable {
                val elements: Array[StackTraceElement] = getBackTrace(caller, arguments(0).asInstanceOf[Rep[Throwable]]);
                if (elements != null) {
                    return unit(elements.length: Integer);
                }
                return unit(0: Integer);
            }
        });
        addDelegate(findMethod(classOf[Throwable], "getStackTraceElement", classOf[Int]), new InterpreterCallable() {

            //@Override
            def invoke(caller: InterpreterFrame, method: ResolvedJavaMethod, arguments: Array[Rep[Object]]): Rep[Object] = {//} throws Throwable {
                val elements: Array[StackTraceElement] = getBackTrace(caller, arguments(0).asInstanceOf[Rep[Throwable]]);
                if (elements != null) {
                    val index: Integer = arguments(0).asInstanceOf[Integer];
                    if (index != null) {
                        return unit(elements(index.intValue));
                    }
                }
                return unit(null);
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

    protected def resolveRootMethod(): ResolvedJavaMethod = {
        try {
            return metaAccessProvider.getResolvedJavaMethod(classOf[BytecodeInterpreter_Impl].getDeclaredMethod("execute", classOf[Method], classOf[Array[Object]]));
        } catch {
            case e: Exception =>
            throw new RuntimeException(e);
        }
    }

    protected def findMethod(clazz: Class[_], name: String, parameters: Class[_]*): Method = {
        try {
            return clazz.getDeclaredMethod(name, parameters:_*);
        } catch {
            case e: Exception =>
            throw new RuntimeException(e);
        }
    }

    private def handleThrowable(root: InterpreterFrame, frame: InterpreterFrame, t: Rep[Throwable]): InterpreterFrame = {
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

    private def updateStackTrace(frame: InterpreterFrame, t: Rep[Throwable]) {
        val elements: Array[StackTraceElement] = getBackTrace(frame, t);
        if (elements != null) {
            setStackTrace(frame, t, elements);
            setBackTrace(frame, t, null);
        } else {
            setBackTrace(frame, t, createStackTraceElements(frame));
        }
    }

    private def setStackTrace(frame: InterpreterFrame, t: Rep[Throwable], stackTrace: Array[StackTraceElement]): Unit = {
        runtimeInterface.setFieldObject(unit(stackTrace), t, findThrowableField(frame, "stackTrace"));
    }

    private def getBackTrace(frame: InterpreterFrame, t: Rep[Throwable]): Array[StackTraceElement] = {
        val value = runtimeInterface.getFieldObject(t, findThrowableField(frame, "backtrace"));
        if (value.isInstanceOf[Array[StackTraceElement]]) {
            return value.asInstanceOf[Array[StackTraceElement]];
        }
        return null;
    }

    private def setBackTrace(frame: InterpreterFrame, t: Rep[Throwable], backtrace: Array[StackTraceElement]): Unit = {
        runtimeInterface.setFieldObject(unit(backtrace), t, findThrowableField(frame, "backtrace"));
    }

    private def resolveExceptionHandlers(frame: InterpreterFrame, bci: Int, t: Rep[Throwable]): ExceptionHandler = {
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

    protected def createStackTraceElements(frame: InterpreterFrame): Array[StackTraceElement] = {
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

    protected def filterStackElement(frame: InterpreterFrame): Boolean = {
        return classOf[Throwable].isAssignableFrom(frame.getMethod().holder().toJava());
    }

    protected def findThrowableField(frame: InterpreterFrame, name: String): ResolvedJavaField = {
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


}


