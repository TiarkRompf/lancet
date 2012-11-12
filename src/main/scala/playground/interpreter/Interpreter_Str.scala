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


  - unique ids for blocks (including inlined)
  - support exceptions


  - generate and compile working scala
  - compile to js, koch snowflake
  - generate graal ir

  - lms: structured nodes, simplify, object abstraction
  - join control flow, lattice ops


  - run compiled scala code
  - constant pool
  - OptiML api

*/


class BytecodeInterpreter_Opt extends BytecodeInterpreter_Str with RuntimeUniverse_Opt {
    override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m)
    override def objectGetClass(receiver: Rep[Object]): Option[Class[_]] = {
      eval(receiver) match {
        case Partial(fs) => 
          val Const(clazz: Class[_]) = eval(fs("clazz"))
          Some(clazz)
        case Const(x) =>
          val clazz = x.getClass
          Some(clazz)
        case _ =>
          None
      }        
    }




    var worklist: IndexedSeq[InterpreterFrame] = Vector.empty

    var budget = 50000

    var emitControlFlow = true
    var emitRecursive = false

    def exec(frame: InterpreterFrame): Rep[Unit] = { // called internally to initiate control transfer
      
      if (budget <= 0) {
        println("// *** BUDGET EXCEEDED ***")
        return unit(().asInstanceOf[Object]).asInstanceOf[Rep[Unit]]
      }

      val method = frame.getMethod()
      if (!emitRecursive && getContext(frame).drop(1).exists(_.getMethod() == method)) { // recursive (TODO: faster test)
        println("// *** RECURSIVE: "+method+" ***")
        return unit(().asInstanceOf[Object]).asInstanceOf[Rep[Unit]]
      }

      budget -= 1
      
      // decision to make: explore block afresh or generate call to existing one

      worklist = worklist :+ (frame.asInstanceOf[InterpreterFrame_Str].copy)

      if (emitControlFlow && worklist.tail.nonEmpty)
        reflect[Unit]("goto "+contextKey(frame))
      else
        unit(().asInstanceOf[Object]).asInstanceOf[Rep[Unit]]
    }


    // TODO: can't translate blocks just like that to Scala methods: 
    // next block may refer to stuff defined in earlier block (need nesting, 
    // but problem with back edges)

    // may need different treatment for intra-procedural blocks and function
    // calls: want to inline functions but not generally duplicate local blocks

    def loop(root: InterpreterFrame): Unit = {// throws Throwable {

      val info = new scala.collection.mutable.HashMap[String, Int]

      while (worklist.nonEmpty) {
        var frame = worklist.head
        worklist = worklist.tail

        val key = contextKey(frame)
        val seen = info.getOrElse(contextKey(frame), 0)

        info(key) = seen + 1

        if (seen > 0) {
          println("// *** SEEN " + seen + ": " + key)
        }

        val seenEnough = seen > 3  // TODO: this is just a random cutoff, need to do fixpoint iteration

        if (!seenEnough && frame.getParentFrame != null) {
          val bci = frame.getBCI()
          val bs = new BytecodeStream(frame.getMethod.code())
          //bs.setBCI(globalFrame.getBCI())

          def frameStr(frame: InterpreterFrame) = getContext(frame).map(frame => ("" + frame.getBCI + ":" + frame.getMethod() + frame.getMethod().signature().asString()).replace("HotSpotMethod",""))

          if (emitControlFlow) {
            println("// *** begin block " + key)
            //println("// *** stack " + frame.asInstanceOf[InterpreterFrame_Str].locals.mkString(","))
          }
          executeBlock(frame, bs, bci)
        }
      }
    }



}


// TODO: straightforward compilation

final class BytecodeInterpreter_Simple extends BytecodeInterpreter_Str with RuntimeUniverse_Simple {


    var worklist: IndexedSeq[InterpreterFrame] = Vector.empty

    var emitControlFlow = true

    val info = new scala.collection.mutable.HashMap[String, Int]
    var count = 0

    def exec(frame: InterpreterFrame): Rep[Unit] = { // called internally to initiate control transfer

      val key = contextKey(frame)
      val id = info.getOrElseUpdate(key, {
        val id = count
        count += 1

        // TODO: copy the whole stack: not ideal

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


    def loop(root: InterpreterFrame): Unit = {

      while (worklist.nonEmpty) {
        var frame = worklist.head
        worklist = worklist.tail

        val key = contextKey(frame)
        val id = info(key)

        println("// *** begin block " + key)
        val params = frame.getReturnValue()::getContext(frame).dropRight(1).flatMap(_.asInstanceOf[InterpreterFrame_Str].locals)
        val paramsStr = params.map(x => if (x eq null) "?" else x.toString+":"+x.typ)
        println("def block_"+id+"("+paramsStr.mkString(",")+"): Any = {")

        if (frame.getParentFrame != null) { // don't eval root frame
          val bci = frame.getBCI()
          val bs = new BytecodeStream(frame.getMethod.code())
          //bs.setBCI(globalFrame.getBCI())
          val res = executeBlock(frame, bs, bci)
          println(res)
        } else {
          println("// returned to root")
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

      val (src0, res) = ("", {//captureOutputResult { 

        val arg = reflect[A]("ARG")

        execute(f.getClass.getMethod("apply", manifest[A].erasure), Array[Rep[Object]](unit(f),arg.asInstanceOf[Rep[Object]])(repManifest[Object]))

      })

      val (source, _) = captureOutputResult {
      
        def classStr(x: Class[_]): String = if (x.isArray()) "Array["+classStr(x.getComponentType)+"]" else x.getName match {
          case "int" => "Int"
          case "char" => "Char"
          case "long" => "Long"
          //TODO
          case s => s
        }

        val cst = constantPool.zipWithIndex.map(p=>"CONST_"+p._2+": "+classStr(p._1.getClass)).mkString(",")

        println("// constants: " + constantPool.toArray.deep.mkString(","))
        println("class Generated("+ cst +") extends ("+manifest[A]+"=>"+manifest[B]+"){")
        println("import sun.misc.Unsafe")
        println("val unsafe = { val fld = classOf[Unsafe].getDeclaredField(\"theUnsafe\"); fld.setAccessible(true); fld.get(classOf[Unsafe]).asInstanceOf[Unsafe]; }")
        println("type char = Char")

        println("def apply(ARG: "+manifest[A]+"): "+manifest[B]+" = { object BODY {")

        println(src0)

        println("val RES = " + res)
        println("}; BODY.RES }")
        println("}")
      }

      println(source)

      ScalaCompile.compile[A,B](source, "Generated", constantPool.toList)
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


            // why

            return popAsObject(rootFrame, signature.returnKind()); // DOESN'T WORK???
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
        loop(root);
    }

    def loop(root: InterpreterFrame): Unit




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


    def checkCastInternal(typ: ResolvedJavaType, value: Rep[Object]): Rep[Object] =
      reflect[Object]("checkCast("+typ.toJava+","+value+")")
 
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
