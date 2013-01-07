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



//@SuppressWarnings("static-method")
final class BytecodeInterpreter_Impl extends InterpreterUniverse_Impl with BytecodeInterpreter_Common {

    import BytecodeInterpreter._

    def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Impl(m)


    // ---------- high level execution loop ----------

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


            var rootFrame: InterpreterFrame_Impl = null // nativeFrame
            if (rootFrame == null) {
              rootFrame = new InterpreterFrame_Impl(rootMethod, signature.argumentSlots(true));
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
        if (TRACE) {
            traceCall(frame, "RootCall");
        }
        exec(frame)
        loop(root);
    }

    var globalFrame: InterpreterFrame = _

    def exec(frame: InterpreterFrame): Unit = { // called internally to initiate control transfer
      globalFrame = frame // will continue execution at `frame`
    }

    private def loop(root: InterpreterFrame): Unit = {// throws Throwable {
      while (globalFrame != root) {
        val bs = new BytecodeStream(globalFrame.getMethod.code())
        //bs.setBCI(globalFrame.getBCI())
        executeBlock(globalFrame, bs, globalFrame.getBCI())
      }
    }


/*

    def executeRoot(root: InterpreterFrame, frame: InterpreterFrame): Unit = { // throws Throwable {
        // TODO reflection redirection
        var prevFrame: InterpreterFrame = frame;
        var currentFrame: InterpreterFrame = frame;
        var bs: BytecodeStream = new BytecodeStream(currentFrame.getMethod().code());
        if (TRACE) {
            traceCall(frame, "RootCall");
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
                    //case NEXT =>
                    //    bs.next();
                    case RETURN =>
                        return popFrame(frame);
                    case CALL =>
                        if (callFrame != null) // null for native calls
                          return allocateFrame(frame, bs);
                        else
                          bs.next()
                    //case BRANCH =>
                    //    bs.setBCI(bs.readBranchDest());
                    //case BRANCH_COND =>
                    //    if (branchCondition)
                    //      bs.setBCI(bs.readBranchDest());
                    //    else
                    //      bs.next()
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
*/



    // ---------- statement level ----------

    def lookupSearch(bs: BytecodeStream, key: Int): Int = {
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
    }

    def tableSearch(bs: BytecodeStream, index: Int): Int = {
        val switchHelper = new BytecodeTableSwitch(bs, bs.currentBCI());

        val low = switchHelper.lowKey();
        val high = switchHelper.highKey();

        assert(low <= high);

        if (index < low || index > high) {
            return switchHelper.defaultTarget();
        } else {
            return switchHelper.targetAt(index - low);
        }
    }

    def checkCastInternal(typ: ResolvedJavaType, value: Object): Object =
      typ.toJava.cast(value).asInstanceOf[Object]


    // called by invokeVirtual

    def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame = {// throws Throwable {
        val receiver: Object = nullCheck(parent.peekReceiver(m));

        val method: ResolvedJavaMethod = resolveType(parent, receiver.getClass()).resolveMethodImpl(m);

        if (method == null) {
            throw new AbstractMethodError();
        }

        return invoke(parent, method, receiver, true);
    }

    
    // called internally by invoke

    def invokeDirect(parent: InterpreterFrame, method: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame = {// throws Throwable {
        //return parent.create(method, hasReceiver);
        return parent.create(method, hasReceiver, 0, true);
    }

}