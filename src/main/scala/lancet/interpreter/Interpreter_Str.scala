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

import java.lang.reflect.{Array=>jlrArray,_};
import java.util.{Vector=>_,_};
import sun.misc._;

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;

// straightforward compilation

final class BytecodeInterpreter_Simple extends BytecodeInterpreter_Str with RuntimeUniverse_Simple {


    var worklist: IndexedSeq[InterpreterFrame] = Vector.empty

    var emitControlFlow = true

    val info = new scala.collection.mutable.HashMap[String, Int]
    var count = 0

    def exec(frame: InterpreterFrame): Rep[Unit] = { // called internally to initiate control transfer

      if (frame.getParentFrame == null) { // TODO: cleanup?
        val p = popAsObject(frame, frame.getMethod.signature.returnKind())
        return reflect[Unit]("(RES = "+p+") // return to root")
      }

      if (getContext(frame).drop(1).exists(_.getMethod() == frame.getMethod)) { // recursive (TODO: faster test)
        println("// *** RECURSIVE: "+frame.getMethod+" ***")
        return reflect[Unit]("throw new Exception(\"RECURSIVE: "+frame.getMethod+"\")")
      }


      // decision to make: explore block afresh or generate call to existing one
      // --> in this version, we explore blocks once for each method call path
      //     and generate calls each time

      val key = contextKey(frame)
      val id = info.getOrElseUpdate(key, {
        val id = count
        count += 1

        // TODO: copy the whole stack: not ideal (handling of root buggy, too)

        def freshFrame(frame: InterpreterFrame): InterpreterFrame_Str = if (frame eq null) null else {
          val frame2 = frame.asInstanceOf[InterpreterFrame_Str].copy2(freshFrame(frame.getParentFrame))
          val depth = frame2.depth
          
          def copyTypeRep(x: Rep[AnyRef]): TypeRep[AnyRef] = (x match { case null => typeRep[Any] case x => x.typ }).asInstanceOf[TypeRep[AnyRef]]

          // use fresh argument symbols
          for (i <- 0 until frame2.locals.length)
            frame2.locals(i) = new Rep[Object]("p"+depth+"_"+i)(copyTypeRep(frame2.locals(i)))

          frame2.returnValue = new Rep[Object]("r")(copyTypeRep(frame2.returnValue))
          frame2
        }


        val frame2 = freshFrame(frame)

        worklist = worklist :+ frame2
        id
      })



      val args = frame.getReturnValue()::getContext(frame).dropRight(1).flatMap(_.asInstanceOf[InterpreterFrame_Str].locals)
      reflect[Unit]("block_"+id+"("+args.mkString(","),") // "+key)
    }


    def loop(root: InterpreterFrame, main: InterpreterFrame): Unit = {

      pushAsObjectInternal(root, main.getMethod.signature().returnKind(), reflect[Object]("null // stub return value "+main.getMethod.signature().returnKind())); // TODO: cleanup?

      while (worklist.nonEmpty) {
        var frame = worklist.head
        worklist = worklist.tail

        val key = contextKey(frame)
        val id = info(key)

        println("// *** begin block " + key)
        val params = frame.getReturnValue()::getContext(frame).dropRight(1).flatMap(_.asInstanceOf[InterpreterFrame_Str].locals)
        val paramsStr = params.map(x => if (x eq null) "?" else x.toString+":"+x.typ)
        println("def block_"+id+"("+paramsStr.mkString(",")+"): Any = {")

        if (frame.getParentFrame != null) { // don't eval root frame -- careful, this is a copy of it!
          val bci = frame.getBCI()
          val bs = new BytecodeStream(frame.getMethod.code())
          //bs.setBCI(globalFrame.getBCI())
          try {
            val res = executeBlock(frame, bs, bci)
            println(res)
          } catch {
            case e: InterpreterException =>
              println("// caught " + e)
              println("throw "+e.cause+".asInstanceOf[Throwable]")
          }
        } else {
          println("// shouldn't reach here..")
          println("// returned to root")
          println("// rval: " + frame.asInstanceOf[InterpreterFrame_Str].returnValue)
        }

        println("}")
      }
    }


}



//@SuppressWarnings("static-method")
trait BytecodeInterpreter_Str extends InterpreterUniverse_Str with BytecodeInterpreter_Common {

    import BytecodeInterpreter._

    def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Str(m)

    override def trace(level: Int, message: String)  = super.trace(level, "// " + message)


    // ---------- high level execution loop ----------

    def compile[A:Manifest,B:Manifest](f: A=>B): A=>B = {

      //def captureOutputResult[T](x:T) = ("", x)

      val (src0, res) = captureOutputResult { 

        val arg = reflect[A]("ARG")

        execute(f.getClass.getMethod("apply", manifest[A].erasure), Array[Rep[Object]](unit(f),arg.asInstanceOf[Rep[Object]])(repManifest[Object]))

      }

      val (source, _) = captureOutputResult {
      
        def classStr(x: Class[_]): String = if (x.isArray()) "Array["+classStr(x.getComponentType)+"]" else x.getName match {
          case "int" => "Int"
          case "char" => "Char"
          case "long" => "Long"
          //TODO/FIXME
          case s if !Modifier.isPublic(x.getModifiers) => "Object /*" + s + "*/" //s <-- class may be private...
          case s => s
            val params = x.getTypeParameters
            if (params.length == 0) s
            else s + "[" + params.map(x=>"_").mkString(",") + "]"
        }

        def manifestStr(x: Manifest[_]) = classStr(x.erasure)

        val (maStr, mbStr) = (manifestStr(manifest[A]), manifestStr(manifest[B]))

        val cst = constantPool.zipWithIndex.map(p=>"CONST_"+p._2+": "+classStr(p._1.getClass)).mkString(",")

        println("// constants: " + constantPool.toArray.deep.mkString(",").replace("\n","\\n"))
        println("class Generated("+ cst +") extends ("+maStr+"=>"+mbStr+"){")
        println("import sun.misc.Unsafe")
        println("val unsafe = { val fld = classOf[Unsafe].getDeclaredField(\"theUnsafe\"); fld.setAccessible(true); fld.get(classOf[Unsafe]).asInstanceOf[Unsafe]; }")
        println("type char = Char")
        println("def WARN = assert(false, \"WARN\")")
        println("def ERROR = assert(false, \"ERROR\")")

        println("def apply(ARG: "+maStr+"): "+mbStr+" = { object BODY {")
        println("  var RES = null.asInstanceOf["+mbStr+"]")

        println(indented(src0.trim))

        println("}; BODY.RES }")
        println("}")
      }

      println(source)

      def specCls(x: AnyRef): (AnyRef,Class[_]) = {
        val cls = x.getClass
        if (Modifier.isPublic(cls.getModifiers)) (x,cls) else (x,classOf[Object])
      }

      ScalaCompile.compile[A,B](source, "Generated", constantPool.map(x=>specCls(x)).toList)
    }

    //@Override
    def execute(method: ResolvedJavaMethod, boxedArguments: Array[Rep[Object]]): Rep[Object] = {// throws Throwable {
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
              rootFrame = new InterpreterFrame_Str(rootMethod, signature.argumentSlots(true)+1);
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


            return popAsObject(rootFrame, signature.returnKind()); // only works if rootFrame not copied internally...
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

    def execute(javaMethod: Method, boxedArguments: Array[Rep[Object]]): Rep[Object] = {// throws Throwable {
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
        loop(root, frame);
    }

    def loop(root: InterpreterFrame, main: InterpreterFrame): Unit




    def getContext(frame: InterpreterFrame): scala.List[InterpreterFrame] =
      if (frame == null) Nil else frame :: getContext(frame.getParentFrame)

    def contextKey(frame: InterpreterFrame) = getContext(frame).map(frameKey).mkString(" // ")

    def frameKey(frame: InterpreterFrame) = ("" + frame.getBCI + ":" + frame.getMethod() + frame.getMethod().signature().asString()).replace("HotSpotMethod","")


    override def retn() = local { (frame, bs) =>

      // create copy -- will be pushing values into parent frame !!
      val parentFrame = frame.getParentFrame.asInstanceOf[InterpreterFrame_Str].copy
      val returnValue = frame.getReturnValue()
      popFrame(frame)
      pushAsObjectInternal(parentFrame, frame.getMethod.signature().returnKind(), returnValue);

      //println("### return "+contextKey(frame))

      exec(parentFrame)
    }


    // ---------- block / statement level ----------

    def lookupSearch(bs: BytecodeStream, key: Rep[Int]): Int = {reflect[Int]("lookupSearch");0}/*{
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

    def tableSearch(bs: BytecodeStream, index: Rep[Int]): Int = {reflect[Int]("tableSearch");0}/*{
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
            val method = resolveType(parent, clazz).resolveMethodImpl(m);
            return invokeDirect(parent, method, true)
          case _ =>
        }


        // TODO: will require registering an assumption ...
        val unique = null//m.holder.uniqueConcreteMethod(m)
        if (unique ne null) {
          println("// unique method: "+m)
          return return invokeDirect(parent, unique, true)
        }


        //val method: ResolvedJavaMethod = resolveType(parent, receiver.getClass()).resolveMethodImpl(m);

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
