package optiml
package macros

import lancet.api._
import lancet.interpreter._
import lancet.core._

import scala.virtualization.lms.internal.{GenericFatCodegen}
import scala.virtualization.lms.common._

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config

//import ppl.dsl.optiml.{Vector,DenseVector,DenseMatrix,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}

import OptiMLLancetRunner._

object IndexVectorMacros extends OptiMLRunner.ClassMacros {
  val targets = List(classOf[optiml.library.IndexVectorRange])
  import OptiMLRunner._ //{Rep,reflect,mtr,infix_relax,decompileFun}

  def construct[T](self: Rep[IndexVectorRange], f: Rep[Int=>T]): Rep[DenseVector[T]] = {
    Console.println("catch indexvector_construct")
    implicit val mf = manifest[Int].asInstanceOf[Manifest[T]] //FIXME: generic types
    val f1 = decompileFun(f)(typeRep[Int],mtr[T])
    OptiMLRunner.indexvector_construct(OptiMLRunner.indexVecRangeToInterface(self),f1) 
  }

  def construct2[T](self: Rep[IndexVectorRange], f: Rep[Int=>T]): Rep[DenseVector[T]] = {
    Console.println("catch indexvector_construct2")
    implicit val mf = manifest[DenseVector[Double]].asInstanceOf[Manifest[T]] //FIXME: generic types
    val f1 = decompileFun(f)(typeRep[Int],mtr[T])
    OptiMLRunner.indexvector_construct(OptiMLRunner.indexVecRangeToInterface(self),f1) 
  }
  
  def constructRows[T](self: Rep[IndexVectorRange], f: Rep[Int => DenseVector[T]]): Rep[DenseMatrix[T]] = {
    try {
      Console.println("catch indexvector_construct_rows")
      implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
      val retType = TypeRep("generated.scala.DenseVectorDouble")(manifest[DenseVector[T]])
      val block = decompileFun(f)(intType,retType)      
      OptiMLRunner.indexvector2_construct_vectors_wildcard2(OptiMLRunner.indexVecRangeToInterface(self),block) 
    } catch {
      case e => e.printStackTrace; throw e
    }    
  }  
}


