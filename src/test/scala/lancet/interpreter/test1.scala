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

    class Foo {
      def bar(x: Int) = { println("hello: "+x); 9 }
    }

    val o = new Foo

    val runtime = HotSpotGraalRuntime.getInstance().getRuntime();

    val cls = o.getClass
    val reflectMeth = cls.getDeclaredMethod("bar", classOf[Int])
    val method = runtime.lookupJavaMethod(reflectMeth)

    val it = Lancet.newInterpreter
    it.TRACE = true
    it.TRACE_BYTE_CODE = true

    val res = it.execute(method, Array[Object](o, 8:Integer))
    
    println("res: " + res)
  }

/*
  // compile simple
  def testB = withOutFileChecked(prefix+"B") {

    final class Foo { // making class final -- Simple compiler can't resolve call otherwise 
      def bar(x: Int) = { System.out.println("hello: "+x); 9 }
    }

    val o = new Foo
    val it = Lancet.newCompilerSimple

    val f = it.compile((x:Int) => o.bar(8))
    println(f(7))

  }
*/
  

/*
  this takes quite long. profiling data: 73s / 2013-01-30
    total             73s
    allLubs           23s
    String.replace    19s
    contextKey        10s
    compile           7s
*/

/*
  // TODO: revisit once we have a better handle on path dependent conditionals

  // compile optimized
  def testC = withOutFileChecked(prefix+"C") {
    //assert(false) // crashes the vm at the moment

    class Foo {
      def bar(x: Int) = { System.out.println("hello: "+x); 9 }
    }

    val o = new Foo
    val it = Lancet.newCompilerOpt

    //it.emitControlFlow = false

    val f = it.compile((x:Int) => o.bar(8))
    println(f(7))
  }
*/

}