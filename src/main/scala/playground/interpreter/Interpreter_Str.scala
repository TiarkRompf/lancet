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
import java.util.{Vector=>_,_};
import sun.misc._;

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;

/* TODO:

  loops/blocks
    evaluate each block only once -- worklist
  
  method calls
    either inline (default) or compile separately


  generate executable code
    private fields? --> unsafe ops


  abstract interpretation / lattice ops

*/




//@SuppressWarnings("static-method")
final class BytecodeInterpreter_Str extends InterpreterUniverse_Str with BytecodeInterpreter_Common {

    import BytecodeInterpreter._

    def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Str(m)

    override def trace(level: Int, message: String)  = super.trace(level, "// " + message)


    // ---------- high level execution loop ----------

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


            var rootFrame: InterpreterFrame_Str = null // nativeFrame
            if (rootFrame == null) {
              rootFrame = new InterpreterFrame_Str(rootMethod, signature.argumentSlots(true));
              rootFrame.pushObject(unit(this));
              rootFrame.pushObject(unit(method));
              rootFrame.pushObject(unit(boxedArguments));
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
            pushAsObject(rootFrame, Kind.Object, unit(boxedArguments(index)));
            index += 1;
        }

        var i = 0
        while (index < boxedArguments.length) {
            pushAsObject(rootFrame, signature.argumentKindAt(i), unit(boxedArguments(index)));
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


    var worklist: IndexedSeq[InterpreterFrame] = Vector.empty

    def exec(frame: InterpreterFrame): Rep[Unit] = { // called internally to initiate control transfer
      
      // decision to make: explore block afresh or generate call to existing one

      worklist = worklist :+ (frame.asInstanceOf[InterpreterFrame_Str].copy)
      reflect("block_"+frame.getBCI()+"() // "+frame.getMethod + " - " + frame.getMethod().signature().asString())
      //unit(().asInstanceOf[Object]).asInstanceOf[Rep[Unit]]
    }



    // TODO: can't translate blocks just like that to Scala methods: 
    // next block may refer to stuff defined in earlier block (need nesting, 
    // but problem with back edges)

    // may need different treatment for intra-procedural blocks and function
    // calls: want to inline functions but not generally duplicate local blocks

    private def loop(root: InterpreterFrame): Unit = {// throws Throwable {
      while (worklist.nonEmpty) {
        var frame = worklist.head
        worklist = worklist.tail

        if (frame.getParentFrame != null) {
          val bci = frame.getBCI()
          val bs = new BytecodeStream(frame.getMethod.code())
          //bs.setBCI(globalFrame.getBCI())
//          println("def block_"+bci+"() { // *** begin block " + bci + " " + frame.getMethod + " - " + frame.getMethod().signature().asString())
          println("// *** begin block " + bci + " " + frame.getMethod + " - " + frame.getMethod().signature().asString())          
          println("// *** stack " + frame.asInstanceOf[InterpreterFrame_Str].locals.mkString(","))
            executeBlock(frame, bs, bci)
//          println("} // *** end block " + bci + " " + frame.getMethod + " - " + frame.getMethod().signature().asString())
        }
      }
    }


  override def retn() = local { (frame, bs) =>

    // create copy -- will be pushing values into parent frame !!

    val parentFrame = frame.getParentFrame.asInstanceOf[InterpreterFrame_Str].copy


    //println(frame.getMethod)
    //println(frame)
    //println(globalFrame)

    val returnValue = frame.getReturnValue()
    popFrame(frame)
/*    
    println("returning from " + frame.getMethod + " to " + parentFrame.getMethod + 
      " bci " + parentFrame.getBCI + 
      " tos " + parentFrame.getStackTop + 
      " space " + parentFrame.numLocals + 
      " value " + returnValue + 
      " kind " + frame.getMethod.signature().returnKind)
*/
    pushAsObjectInternal(parentFrame, frame.getMethod.signature().returnKind(), returnValue);

    exec(parentFrame)
  }


    // ---------- block / statement level ----------

    def lookupSearch(bs: BytecodeStream, key: Rep[Int]): Int = {reflect("lookupSearch");0}/*{
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

    def tableSearch(bs: BytecodeStream, index: Rep[Int]): Int = {reflect("tableSearch");0}/*{
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


    def checkCastInternal(typ: ResolvedJavaType, value: Rep[Object]): Rep[Object] =
      reflect("checkCast("+typ.toJava+","+value+")")
 
    // called by invokeVirtual

    def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame = {// throws Throwable {
        val receiver = nullCheck(parent.peekReceiver(m));

        // TODO/FIXME

        val parameters = popArgumentsAsObject(parent, m, true);
        val returnValue = runtimeInterface.invoke(m, parameters);
        pushAsObject(parent, m.signature().returnKind(), returnValue);
        null

        /*val method: ResolvedJavaMethod = resolveType(parent, receiver.getClass()).resolveMethodImpl(m);

        if (method == null) {
            throw new AbstractMethodError();
        }

        return invoke(parent, method, receiver);*/
    }


    // called internally by invoke

    def invokeDirect(parent: InterpreterFrame, method: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame = {// throws Throwable {
        //return parent.create(method, hasReceiver);
        return parent.create(method, hasReceiver, 0, true);
    }
}
