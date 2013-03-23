package optiml
package macros

import lancet.api._
import lancet.interpreter._
import lancet.core._

import scala.virtualization.lms.internal.{GenericFatCodegen}
import scala.virtualization.lms.common._

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config

import ppl.dsl.optiml.{Vector,DenseVector,DenseMatrix,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}

import OptiMLLancetRunner._

object DenseMatrixMacros extends OptiMLRunner.ClassMacros {
  val targets = List(classOf[optiml.library.DenseMatrix[_]])
  import OptiMLRunner.{Rep,reflect,mtr,infix_relax}
  
  def numRows[T](self: Rep[DenseMatrix[T]]): Rep[Int] = {
    Console.println("catch matrix_numrows")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.densematrix_numrows(self)
  }  

  // might need to add a specialized version of this for DenseMatrix in optiml for now (to avoid dealing with the real function type f: Interface[T] => B)
  // def mapRowsToVector[T,B](self: Rep[DenseMatrix[T]], f: Rep[DenseVector[T]] => Rep[B]): Rep[DenseVector[B]] = {
  //   Console.println("catch matrix_maprowstovector")
  //   implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
  //   implicit val mfb = manifest[Double].asInstanceOf[Manifest[B]] //FIXME: generic types
  //   implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[B,DenseVector[B]]]    
  //   OptiMLRunner.matrix_maprowstovec[T,B,DenseVector[B]](OptiMLRunner.denseMatToInterface(self), f, OptiMLRunner.unit(true))
  // }  
  
  def pprint[T](self: Rep[DenseMatrix[T]]): Rep[Unit] = {
    Console.println("catch matrix_pprint")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.matrix_pprint(OptiMLRunner.denseMatToInterface(self))
  }    
}

