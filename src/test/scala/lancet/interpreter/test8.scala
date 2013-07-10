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

class TestInterpreter8 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-8"

  import java.math.BigInteger




  def test1 = withOutFileChecked(prefix+"bigint1") {
    val it = new Compiler { def slowpath2() {} }

    abstract class SafeInt { def toBigInt: BigInteger }
    case class Small(x: Int) extends SafeInt { def toBigInt = new BigInteger(x.toString) }
    case class Big(x: BigInteger) extends SafeInt { def toBigInt = x }

    def SafeInt(x: Long): SafeInt = 
      if (x >= Int.MinValue && x <= Int.MaxValue) Small(x.toInt)
      else { it.slowpath(); Big(new BigInteger(x.toString)) }

    def infix_*(x: SafeInt, y: SafeInt): SafeInt = (x,y) match {
      case (Small(x),Small(y)) => SafeInt(x.toLong * y.toLong)
      case _ => it.slowpath2(); Big(x.toBigInt.multiply(y.toBigInt))
    }


    def fac(n: Int) = {
      var i = 1
      var prod = SafeInt(1)
      while (i < n) {
        prod = prod * SafeInt(i)
        i += 1
      }
      prod
    }


    val f = it.compile { (x:Int) => 
      val m = fac(x)
      7
    }
    

    f(100)
  }



  class Compiler extends BytecodeInterpreter_TIR_Opt with DefaultMacros {

    initialize()
    emitUniqueOpt = true
    emitRecursive = true
    emitCheckCast = false
    debugBlockKeys = false
    debugReadWrite = false
    debugMethods = false
    debugReturns = false

  }

}
