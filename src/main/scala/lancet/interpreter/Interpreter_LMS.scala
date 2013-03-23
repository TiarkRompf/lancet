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

import java.lang.reflect.{Array=>jlrArray,_}
import java.util.{Vector=>_,List=>_,_}
import java.io.{PrintWriter,StringWriter}
import sun.misc._

import com.oracle.graal.api._
import com.oracle.graal.api.meta._
import com.oracle.graal.hotspot.meta._
import com.oracle.graal.bytecode._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._


//@SuppressWarnings("static-method")
trait BytecodeInterpreter_LMS extends InterpreterUniverse_LMS with BytecodeInterpreter_Common_Compile { self =>

    import BytecodeInterpreter._

    def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_LMS(m)

    override def trace(level: Int, message: String)  = super.trace(level, "// " + message)

    override def initialize(): Unit = {
      ScalaCompile.reset()
      super.initialize()
    }

    var emitUniqueOpt = false
    var debugGlobalDefs = false
    var debugDepGraph = false

    // ---------- high level execution loop ----------

    /* see test4
    def quoteFun[A:Manifest,B:Manifest](f: A=>B): String = {
      val (src0, res) = captureOutputResult { 

        val arg = reflect[A]("ARG")

        execute(f.getClass.getMethod("apply", manifest[A].erasure), Array[Rep[Object]](unit(f),arg.asInstanceOf[Rep[Object]])(repManifest[Object]))

      }

      "{ (ARG: " + manifest[A] + ") => " + src0 + "}"
    }*/

    def printIndented(str: String)(emit: String => Unit): Unit = {
      val lines = str.split("\n")
      var indent = 1
      for (l0 <- lines) {
        val l = l0.trim
        if (l.length > 0) {
        var open = 0
        var close = 0
        l foreach { case '{' => open += 1 case '}' => close += 1 case _ => }
        val d = if (close == 0) 0 else l.takeWhile(_ == '}').length
        emit("  "*(indent-d) + l)
        indent += (open - close)
        }
      }
    }

    def createCodegen(): GEN_Scala_LMS { val IR: self.type } = 
      new GEN_Scala_LMS { val IR: self.type = self }

    def compile[A:Manifest,B:Manifest](f: A=>B): A=>B = {

      implicit val tp = manifestToTypeRep(manifest[B])
      val (maStr, mbStr) = (manifestStr(manifest[A]), manifestStr(manifest[B]))

      val arg = fresh[Int]
      val y = reify {

        emitString("import sun.misc.Unsafe")
        emitString("val unsafe = { val fld = classOf[Unsafe].getDeclaredField(\"theUnsafe\"); fld.setAccessible(true); fld.get(classOf[Unsafe]).asInstanceOf[Unsafe]; }")
        emitString("type char = Char")
        emitString("def WARN = assert(false, \"WARN\")")
        emitString("def ERROR = assert(false, \"ERROR\")")

        emitString("  var RES = null.asInstanceOf["+mbStr+"]")


        execute(f.getClass.getMethod("apply", manifest[A].erasure), Array[Rep[Object]](unit(f),arg.asInstanceOf[Rep[Object]])(repManifest[Object]))        

        DynExp[B]("RES")
      }

      val codegen = createCodegen()

      VConstantPool = Vector.empty

      // dry run to hash out constant pool ... HACK
      codegen.withStream(new PrintWriter(new StringWriter)) {
        codegen.emitSource(List(arg),y,"Generated",codegen.stream)
      }

      val cst = VConstantPool
    
      val stream = new StringWriter
      codegen.withStream(new PrintWriter(stream)) {
        codegen.emitSource(List(arg),y,"Generated",codegen.stream)
      }


      val source = stream.toString 
      printIndented(source)(Console.println)


      if (debugGlobalDefs) globalDefs.foreach(println)

      if (debugDepGraph) {
        val exp = new ExportGraph { val IR: self.type = self }
        println("dumping graph to: "+this.getClass.getName)
        exp.exportGraph(this.getClass.getName)(y.res)
      }


      val f2 = ScalaCompile.compile[A,B](source, "Generated", cst.map(x=>specCls(x._2)).toList)

      f2
    }

    //@Override
    def execute(method: ResolvedJavaMethod, boxedArguments: Array[Rep[Object]]): Rep[Object] = {// throws Throwable {
        try {
            val receiver: Boolean = hasReceiver(method);
            val signature: Signature = method.getSignature();
            assert(boxedArguments != null);
            assert(signature.getParameterCount(receiver) == boxedArguments.length);

            if (TRACE) {
                //if (nativeFrame == null) {
                    trace(0, "Executing root method " + method);
                //} else {
                //    trace(nativeFrame.depth(), "Executing from native " + method);
                //}
            }


            var rootFrame: InterpreterFrame_Str = null // nativeFrame
            if (rootFrame == null) {
              rootFrame = new InterpreterFrame_Str(rootMethod, signature.getParameterSlots(true)+1);
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


            return popAsObject(rootFrame, signature.getReturnKind()); // only works if rootFrame not copied internally...
        } catch {
            case e: Exception =>
            // TODO (chaeubl): remove this exception handler (only used for debugging)
            throw e;
        }/* finally {
            nativeCallerFrame.set(nativeFrame);
        }*/
    }

    def initializeLocals(rootFrame: InterpreterFrame, method: ResolvedJavaMethod, boxedArguments: Array[Rep[Object]]) {
        val receiver: Boolean = hasReceiver(method);
        val signature: Signature = method.getSignature();
        var index = 0;
        if (receiver) {
            pushAsObject(rootFrame, Kind.Object, boxedArguments(index));
            index += 1;
        }

        var i = 0
        while (index < boxedArguments.length) {
            pushAsObject(rootFrame, signature.getParameterKind(i), boxedArguments(index));
            i += 1
            index += 1
        }
        // push the remaining locals
        rootFrame.pushVoid(rootFrame.stackTos() - rootFrame.getStackTop());
    }

    def execute(javaMethod: Method, boxedArguments: Array[Rep[Object]]): Rep[Object] = {// throws Throwable {
        return execute(metaAccessProvider.lookupJavaMethod(javaMethod), boxedArguments);
    }

    def hasReceiver(method: ResolvedJavaMethod): Boolean = {
        return !Modifier.isStatic(method.getModifiers());
    }

    def executeRoot(root: InterpreterFrame, frame: InterpreterFrame): Unit = { // throws Throwable {
        if (TRACE) {
            traceCall(frame, "RootCall");
        }
        exec(frame)
        loop(root, frame);
    }

    def loop(root: InterpreterFrame, main: InterpreterFrame): Unit




    def getContext(frame: InterpreterFrame): scala.List[InterpreterFrame] =
      if (frame == null) Nil else frame :: getContext(frame.getParentFrame)

    def contextKey(frame: InterpreterFrame) = getContext(frame).map(frameKey).mkString(" // ")

    def frameKey(frame: InterpreterFrame) = ("" + frame.getBCI + ":" + frame.getMethod() + frame.getMethod().getSignature().toString()).replace("HotSpotMethod","")


    override def retn() = local { (frame, bs) =>

      // create copy -- will be pushing values into parent frame !!
      val parentFrame = frame.getParentFrame.asInstanceOf[InterpreterFrame_Str].copy
      val returnValue = frame.getReturnValue()
      popFrame(frame)
      pushAsObjectInternal(parentFrame, frame.getMethod.getSignature().getReturnKind(), returnValue);

      //println("### return "+contextKey(frame))

      exec(parentFrame)
    }


    // ---------- block / statement level ----------

    def lookupSearch(bs: BytecodeStream, key: Rep[Int])(k: Int => Rep[Unit]): Rep[Unit] = {
      val switchHelper = new BytecodeLookupSwitch(bs, bs.currentBCI())

      var low = 0;
      var high = switchHelper.numberOfCases() - 1;

      ???

      /*emitString(""+key+" match {")
      for (i <- low to high) {
        val midVal = switchHelper.keyAt(i);
        emitString("case "+midVal+" => ")
        emitBlock(reify(k(switchHelper.bci() + switchHelper.offsetAt(i)))) // FIXME
      }
      emitString("case _ => ")
      emitBlock(reify(k(switchHelper.defaultTarget)))
      emitString("}")
      liftConst(())*/
    }
    /*{
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

    def tableSearch(bs: BytecodeStream, index: Rep[Int])(k: Int => Rep[Unit]): Rep[Unit] = {
      val switchHelper = new BytecodeTableSwitch(bs, bs.currentBCI());

      val low = switchHelper.lowKey();
      val high = switchHelper.highKey();

      assert(low <= high);

      ???

      /*emitString(""+index+" match {")
      for (i <- low to high) {
        emitString("case "+i+" => ")
        emitBlock(reify(k(switchHelper.targetAt(i-low))))
      }
      emitString("case _ => ")
      emitBlock(reify(k(switchHelper.defaultTarget)))
      emitString("}")

      /*
      if (index < low || index > high) {
          return switchHelper.defaultTarget();
      } else {
          return switchHelper.targetAt(index - low);
      }
      */
      liftConst(())*/
    }


    def checkCastInternal(typ: ResolvedJavaType, value: Rep[Object]): Rep[Object] = {

      val cls = typ.toJava
      val params = cls.getTypeParameters
      val name = if (params.length == 0) cls.getName
                 else cls.getName + "[" + params.map(x=>"_").mkString(",") + "]"

      reflect[Object](value+".asInstanceOf["+name+"] // checkCast")
    }

    // called by invokeVirtual

    def objectGetClass(receiver: Rep[Object]): Option[Class[_]] = None

    def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame = {// throws Throwable {
        val receiver = nullCheck(parent.peekReceiver(m));

        // TODO/FIXME

        // get the receiver's class, if possible

        objectGetClass(receiver) match {
          case Some(clazz) => 
            val method = resolveType(parent, clazz).resolveMethod(m);
            return invokeDirect(parent, method, true)
          case _ =>
        }


        // TODO: will require registering an assumption ...
        val unique = if (emitUniqueOpt) m.getDeclaringClass.findUniqueConcreteMethod(m) else null
        if (unique ne null) {
          emitString("// unique method: "+m+" TODO: guard")
          return invokeDirect(parent, unique, true)
        }

        // TODO: if non unique, may want to switch on all possible targets


        //val method: ResolvedJavaMethod = resolveType(parent, receiver.getClass()).resolveMethod(m);

        val parameters = popArgumentsAsObject(parent, m, true);
        val returnValue = runtimeInterface.invoke(m, parameters);
        pushAsObject(parent, m.getSignature().getReturnKind(), returnValue);
        null

        /*val method: ResolvedJavaMethod = resolveType(parent, receiver.getClass()).resolveMethod(m);

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
