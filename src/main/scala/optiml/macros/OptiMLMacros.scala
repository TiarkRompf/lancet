package optiml
package macros

import lancet.api._
import lancet.interpreter._
import lancet.core._

import scala.virtualization.lms.internal.{GenericFatCodegen}
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config

import ppl.dsl.optiml.{Vector,DenseVector,DenseVectorView,DenseMatrix,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}

import optiml.library.OptiMLCompanion
import OptiMLLancetRunner._

object OptiMLMacros extends OptiMLRunner.ClassMacros {
  val targets = List(classOf[OptiMLCompanion])
  import OptiMLRunner._ //{Rep,reflect,mtr,infix_relax}
  
  def readMatrix(self: Rep[OptiMLCompanion], path: Rep[String]): Rep[DenseMatrix[Double]] = {
    Console.println("catch readMatrix")
    OptiMLRunner.readMatrix(path)
  }
  
  // using Seq[Rep[Any]] causes an assertion to fail in DeliteSupport
  // def tic(self: Rep[OptiMLCompanion], deps: Rep[Seq[Any]]): Rep[Unit] = {
  //   Console.println("catch tic")
  //   OptiMLRunner.profile_start(OptiMLRunner.strToRepStr("app"),deps)
  // }
  // 
  // def toc(self: Rep[OptiMLCompanion], deps: Rep[Seq[Any]]): Rep[Unit] = {
  //   Console.println("catch toc")
  //   OptiMLRunner.profile_stop(OptiMLRunner.strToRepStr("app"),deps)
  // }  
  
  def index_new(self: Rep[OptiMLCompanion], start: Rep[Int], end: Rep[Int]): Rep[IndexVectorRange] = {
    Console.println("catch index_new")
    OptiMLRunner.indexvector_range(start,end)
  }

  // typing assertion fails for this one. could be that either functions or tuples are getting reified differently than we expect
  // def indexvector_hashreduce[V](self: Rep[OptiMLCompanion], x: Rep[IndexVectorRange], f: Rep[Int] => Rep[Int], map: Rep[Int] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V]): Rep[DenseVector[V]] = {
  //   Console.println("catch indexvector_hashreduce")
  //   implicit val mf = manifest[DenseVector[Double]].asInstanceOf[Manifest[V]] //FIXME: generic types
  //   implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[V]]
  //   OptiMLRunner.indexvector_hashreduce(OptiMLRunner.indexVecRangeToInterface(x),f,map,reduce)    
  // }
  
  def dist[T](self: Rep[OptiMLCompanion], x: Rep[DenseVectorView[T]], y: Rep[DenseVector[T]]): Rep[T] = {
    Console.println("catch dist")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    OptiMLRunner.optila_vector_dist_square(OptiMLRunner.denseViewToInterface(x),OptiMLRunner.denseVecToInterface(y)) 
  }

  // typing assertion fails here too
  def untilconverged[T](self: Rep[OptiMLCompanion], x: Rep[DenseMatrix[T]], tol: Rep[Double], block: Rep[DenseMatrix[T] => DenseMatrix[T]]): Rep[DenseMatrix[T]] = {
    try {
      Console.println("catch untilconverged")
      implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
      //implicit val matOps = OptiMLRunner.repToDenseMatOps(x).asInstanceOf[Rep[DenseMatrix[Double]] => OptiMLRunner.MatOpsCls[Double]]
      implicit val cl = OptiMLRunner.matrixCloneable[Double,DenseMatrix[Double]].asInstanceOf[OptiMLRunner.Cloneable[DenseMatrix[T]]]
      implicit val ar = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
      implicit val diff = (a: Rep[DenseMatrix[T]], b: Rep[DenseMatrix[T]]) => (OptiMLRunner.optila_matrix_dist_square(OptiMLRunner.denseMatToInterface(a),OptiMLRunner.denseMatToInterface(b))(mf,ar,implicitly[SourceContext])).asInstanceOf[Rep[Double]]
      // somehow the default string output is just ppl.dsl.optila.DenseMatrix (without type param)
      val tpe = TypeRep("generated.scala.DenseMatrixDouble")(manifest[DenseMatrix[T]])
      val block1 = decompileFun(block)(tpe,tpe)
      OptiMLRunner.optiml_untilconverged[DenseMatrix[T]](x,(a: Rep[DenseMatrix[T]]) => tol,OptiMLRunner.unit(10),OptiMLRunner.unit(true),block1,diff)
    } catch {
      case e => e.printStackTrace; throw e
    }
  }
  
}

