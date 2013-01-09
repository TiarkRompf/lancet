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
