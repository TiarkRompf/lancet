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

package lancet
package interpreter

import lancet.api._

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

class TestInterpreter1 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-1"

  // interpret
  def testA = withOutFileChecked(prefix+"A") {


import lancet.interpreter._

val it = new BytecodeInterpreter_Exec

it.initialize

it.TRACE = true

it.TRACE_BYTE_CODE = true

def exec(body: () => Any): Any = { val m = body.getClass.getMethod("apply"); println(m); it.execute(m,Array[AnyRef](body)) }

exec(() => println("foo"))

exec { () => try { throw new Exception() } catch { case _ => println("foo") } }

exec(() => throw new Exception)


val sourceFile = new java.io.File("/Users/me/Desktop/tmpstuff/linz/graalvm-truffle/graal/com.oracle.truffle.js.test/src/benchmarks/v8-deltablue.js")

import java.util.Scanner
import java.io.File

val scanner = new Scanner(sourceFile)

val source = scanner.useDelimiter("\\A").next()

import com.oracle.truffle._
import com.oracle.truffle.js._
import com.oracle.truffle.js.compiler._
import com.oracle.truffle.js.nodes.ScriptNode;
import com.oracle.truffle.js.nodes.control._
import com.oracle.truffle.js.runtime._

val globalContext = new JSContext();
globalContext.getGlobalObject().setProperty("alert", globalContext.getGlobalObject().getProperty("print"));
val scriptAst = JSParser.parse(source);
scriptAst.debugPrint
val script = JSCompiler.compileScript(globalContext, scriptAst);
script.run

it.TRACE = false
it.TRACE_BYTE_CODE = false


exec(() => script.run)

// group by call target
val red = it.rawlog.groupBy(x=>x) map { case (k,v) => (k,v.size) }

val res24 = red.toSeq.groupBy { case ((next,cur),n) => cur.f }
res24 foreach { x => println(x._1); println("  "+x._2.mkString("\n  ")) }

// call targets per call site (disregard object ids)
val noids = it.rawlog.map { case (p1,p2) => (p1.f,p2.f) }
val res30 = noids.groupBy(_._2) map { case (k,v) => (k,v.groupBy(_._1).map{case (k,v) => (k,v.size)})}
res30 foreach { x => println(x._1); println("  "+x._2.toSeq.sortBy(_._2).mkString("\n  ")) }


  }

}