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
package advanced

import lancet.api._
import lancet.interpreter._
import lancet.core._

//import ppl.dsl.optiml.{Vector,DenseVector,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config

import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}
import scala.virtualization.lms.internal.{GenericFatCodegen}

import scala.virtualization.lms.common._

class TestDelite1 extends FileDiffSuite {

  val prefix = "test-out/test-delite-1"

  def testA1 = withOutFileChecked(prefix+"A1") {
    object VectorOperatorsRunner extends DeliteTestRunner with OptiMLApplicationRunner with VectorOperators
    trait VectorOperators extends DeliteTestModule with OptiMLApplication {
      def main() {
        
        val v = Vector.rand(1000)

        val vt = v.t
        collect(vt.isRow != v.isRow)

        //val vc = v.clone
        //collect(vc.cmp(v) == true)

        val v2 = Vector(1,2,3,4,5)
        //collect(median(v2) == 3)
        collect(mean(v2) == 3)
        collect(max(v2) == 5)
        collect(min(v2) == 1)
        collect(mean(3,6,2,5) == 4.0)

        println("AA")
        println("BB")
        println("CC")

        mkReport

      }
    }
    DeliteRunner.compileAndTest(VectorOperatorsRunner)
  }
}
