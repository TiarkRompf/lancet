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

object IndexVectorMacros extends OptiMLRunner.ClassMacros {
  val targets = List(classOf[optiml.library.IndexVectorRange])
  import OptiMLRunner._ //{Rep,reflect,mtr,infix_relax,decompileFun}

  def construct[T](self: Rep[IndexVectorRange], f: Rep[Int=>T]): Rep[DenseVector[T]] = {
    Console.println("catch indexvector_construct_rows")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    val f1 = decompileFun(f)(typeRep[Int],mtr[T])
    OptiMLRunner.indexvector_construct(OptiMLRunner.indexVecRangeToInterface(self),f1) 
  }
  
  // same problem with interface function as MatrixMapRowsToVec...
  // def constructRows[T](self: Rep[IndexVectorRange], f: Rep[Int] => Rep[DenseVector[T]]): Rep[DenseMatrix[T]] = {
  //   Console.println("catch indexvector_construct_rows")
  //   implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
  //   OptiMLRunner.indexvector2_construct_vectors_wildcard(OptiMLRunner.indexVecRangeToInterface(self),f) 
  // }
  // 
}


