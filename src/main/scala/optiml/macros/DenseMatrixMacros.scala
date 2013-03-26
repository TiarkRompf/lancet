package optiml
package macros

import lancet.api._
import lancet.interpreter._
import lancet.core._

import scala.virtualization.lms.internal.{GenericFatCodegen}
import scala.virtualization.lms.common._

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config

import ppl.dsl.optiml.{Vector,DenseVector,DenseVectorView,DenseMatrix,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}

import OptiMLLancetRunner._

object DenseMatrixMacros extends OptiMLRunner.ClassMacros {
  val targets = List(classOf[optiml.library.DenseMatrix[_]])
  import OptiMLRunner._ //{Rep,reflect,mtr,infix_relax,decompileFun}
  
  def numRows[T](self: Rep[DenseMatrix[T]]): Rep[Int] = {
    Console.println("catch matrix_numrows")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.densematrix_numrows(self)
  }  

  def getRow[T](self: Rep[DenseMatrix[T]], i: Rep[Int]): Rep[DenseVectorView[T]] = {
    Console.println("catch matrix_getrow")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.densematrix_getrow(self,i)
  }
  
  def mapRowsToVector[T,B](self: Rep[DenseMatrix[T]], f: Rep[DenseVectorView[T] => B]): Rep[DenseVector[B]] = {
    try {
      Console.println("catch matrix_maprowstovector")
      implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
      implicit val mfb = manifest[Double].asInstanceOf[Manifest[B]] //FIXME: generic types
      // implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[B,DenseVector[B]]]    
      val argType = TypeRep("generated.scala.DenseVectorViewDouble")(manifest[DenseVectorView[T]])
      val retType = TypeRep("Double")(manifest[B])
      val block = decompileFun(f)(argType,retType)    
      OptiMLRunner.densematrix_maprowstovector[T,B](self, block, OptiMLRunner.unit(true))
    } catch {
      case e => e.printStackTrace; throw e
    }    
  }  
  
  def pprint[T](self: Rep[DenseMatrix[T]]): Rep[Unit] = {
    Console.println("catch matrix_pprint")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.matrix_pprint(OptiMLRunner.denseMatToInterface(self))
  }    
}

