package optiml
package macros

import lancet.api._
import lancet.interpreter._
import lancet.core._

import scala.virtualization.lms.internal.{GenericFatCodegen}
import scala.virtualization.lms.common._

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config

import ppl.dsl.optiml.{Vector,DenseVector,DenseVectorView,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}

import optiml.library.DenseVectorCompanion
import OptiMLLancetRunner._

object DenseVectorMacros extends OptiMLRunner.ClassMacros {
  val targets = List(classOf[optiml.library.DenseVectorCompanion],classOf[optiml.library.DenseVector[_]],classOf[optiml.library.DenseVectorView[_]])
  //type Rep[T] = VectorOperatorsRunner.Rep[T]
  import OptiMLRunner.{Rep,reflect,mtr,infix_relax}
  
  def rand(self: Rep[DenseVectorCompanion], n: Rep[Int]): Rep[DenseVector[Double]] = {
    Console.println("catch vector_rand")
    OptiMLRunner.densevector_obj_rand(n)
  }
  /*
  def apply[T](self: Rep[DenseVectorCompanion], xs: Rep[Seq[T]]): Rep[DenseVector[T]] = {
    Console.println("catch vector_apply")
    implicit val mf = manifest[Int].asInstanceOf[Manifest[T]] //FIXME: generic types
    val xs1 = reflect[Seq[T]](xs,".asInstanceOf[Seq[Int]]")(mtr[Seq[Int]].relax) // need cast ...
    OptiMLRunner.densevector_obj_fromseq(xs1)
    // TODO: generic types are problematic...
    // require manifest parameter and try to eval that?
    // or use scala reflection?
  }
  */
  def t[T](self: Rep[DenseVector[T]]): Rep[DenseVector[T]] = {
    Console.println("catch vector_t")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.densevector_trans(self)
  }
  def isRow[T](self: Rep[DenseVector[T]]): Rep[Boolean] = {
    Console.println("catch vector_isRow")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.densevector_isrow(self)
  }
  def length[T](self: Rep[DenseVector[T]]): Rep[Int] = {
    Console.println("catch vector_length")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.densevector_length(self)
  }  
  def minIndex[T](self: Rep[DenseVector[T]]): Rep[Int] = {
    Console.println("catch vector_minIndex")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val af = implicitly[Ordering[Double]].asInstanceOf[Ordering[T]] //FIXME: generic types
    implicit val bf = OptiMLRunner.doubleHasMinMax.asInstanceOf[OptiMLRunner.HasMinMax[T]] //FIXME: generic types    
    OptiMLRunner.vector_minindex(OptiMLRunner.denseVecToInterface(self))
  }    
  def Clone[T](self: Rep[DenseVectorView[T]]): Rep[DenseVector[T]] = {
    Console.println("catch vectorview_clone")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_clone[T,DenseVector[T]](OptiMLRunner.denseViewToInterface(self))
  }
  def /[T](self: Rep[DenseVector[T]], b: Rep[T]): Rep[DenseVector[T]] = {    
    Console.println("catch vector_divide_scalar")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_divide_scalar[T,DenseVector[T]](OptiMLRunner.denseVecToInterface(self),b)
  }
  def +[T](self: Rep[DenseVector[T]], b: Rep[DenseVector[T]]): Rep[DenseVector[T]] = {    
    Console.println("catch vector_plus")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_plus[T,DenseVector[T]](OptiMLRunner.denseVecToInterface(self),OptiMLRunner.denseVecToInterface(b))
  }
}

