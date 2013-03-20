package lancet
package advanced

import lancet.api._
import lancet.interpreter._
import lancet.core._

import ppl.dsl.optiml.{Vector,DenseVector,RangeVector,IndexVectorRange}
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