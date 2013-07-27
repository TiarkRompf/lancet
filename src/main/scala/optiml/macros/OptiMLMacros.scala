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

//import ppl.dsl.optiml.{Vector,DenseVector,DenseVectorView,DenseMatrix,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}

import optiml.library.OptiMLCompanion
import OptiMLLancetRunner._

object OptiMLMacros extends OptiMLRunner.ClassMacros {
  val targets = List(classOf[OptiMLCompanion])
  import OptiMLRunner._ //{Rep,reflect,mtr,infix_relax}
  
  def readMatrix(self: Rep[OptiMLCompanion], path: Rep[String]): Rep[DenseMatrix[Double]] = {
    Console.println("catch readMatrix")
    OptiMLRunner.readMatrix(ensureType(path))
  }
  
  def readVector(self: Rep[OptiMLCompanion], path: Rep[String]): Rep[DenseVector[Double]] = {
    Console.println("catch readVector")
    OptiMLRunner.readVector(ensureType(path))
  }
  
  def tic(self: Rep[OptiMLCompanion], dep1: Rep[Any], dep2: Rep[Any]): Rep[Unit] = {
    Console.println("catch tic")
    OptiMLRunner.profile_start(OptiMLRunner.strToRepStr("app"),scala.Seq(dep1,dep2))
  }
  
  def toc(self: Rep[OptiMLCompanion], dep: Rep[Any]): Rep[Unit] = {
    Console.println("catch toc")
    OptiMLRunner.profile_stop(OptiMLRunner.strToRepStr("app"),scala.Seq(dep))
  }  
  
  def index_new(self: Rep[OptiMLCompanion], start: Rep[Int], end: Rep[Int]): Rep[IndexVectorRange] = {
    Console.println("catch index_new")
    OptiMLRunner.indexvector_range(start,end)
  }

  def indexvector_hashreduce(self: Rep[OptiMLCompanion], x: Rep[IndexVectorRange], f: Rep[Int => Int], map: Rep[Int => DenseVector[Double]], reduce: Rep[(DenseVector[Double],DenseVector[Double]) => DenseVector[Double]]): Rep[DenseVector[DenseVector[Double]]] = {
    try {
      Console.println("catch indexvector_hashreduce")
      implicit val mf = manifest[DenseVector[Double]] //FIXME: generic types
      implicit val a = OptiMLRunner.denseVectorArith[Double]
      val block1 = decompileFun(f)(intType,intType)
      val vType = TypeRep("generated.scala.DenseVectorDouble")(mf)
      val block2 = decompileFun(map,1)(intType,vType)
      // val tvType = TypeRep("(generated.scala.DenseVectorDouble,generated.scala.DenseVectorDouble)")(manifest[(V,V)])
      val block3 = decompileFun2(reduce,2)(vType,vType,vType)
      OptiMLRunner.indexvector_hashreduce(OptiMLRunner.indexVecRangeToInterface(x),block1,block2,block3)    
    } catch {
      case e => e.printStackTrace; throw e
    }
  }
  
  def indexvector_hashreduce2(self: Rep[OptiMLCompanion], x: Rep[IndexVectorRange], f: Rep[Int => Int], map: Rep[Int => Int], reduce: Rep[(Int,Int) => Int]): Rep[DenseVector[Int]] = {
    try {
      Console.println("catch indexvector_hashreduce2")
      // implicit val mf = manifest[Int] //FIXME: generic types
      // implicit val a = OptiMLRunner.intArith
      val block1 = decompileFun(f)(intType,intType)
      val block2 = decompileFun(map,1)(intType,intType)
      val block3 = decompileFun2(reduce,2)(intType,intType,intType)
      OptiMLRunner.indexvector_hashreduce(OptiMLRunner.indexVecRangeToInterface(x),block1,block2,block3)    
    } catch {
      case e => e.printStackTrace; throw e
    }
  }  
  
  def exp(self: Rep[OptiMLCompanion], x: Rep[Double]): Rep[Double] = {
    Console.println("catch exp")
    OptiMLRunner.math_exp(x)
  }

  def dist[T](self: Rep[OptiMLCompanion], x: Rep[DenseVectorView[T]], y: Rep[DenseVectorView[T]]): Rep[T] = {
    Console.println("catch dist")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    OptiMLRunner.optila_vector_dist_square(OptiMLRunner.denseViewToInterface(x),OptiMLRunner.denseViewToInterface(y)) 
  }
  
  def dist2[T](self: Rep[OptiMLCompanion], x: Rep[DenseVector[T]], y: Rep[DenseVector[T]]): Rep[T] = {
    Console.println("catch dist2")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    OptiMLRunner.optila_vector_dist_square(OptiMLRunner.denseVecToInterface(x),OptiMLRunner.denseVecToInterface(y)) 
  }
  
  def dist3[T](self: Rep[OptiMLCompanion], x: Rep[DenseMatrix[T]], y: Rep[DenseMatrix[T]]): Rep[T] = {
    Console.println("catch dist3")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    OptiMLRunner.optila_matrix_dist_square(OptiMLRunner.denseMatToInterface(x),OptiMLRunner.denseMatToInterface(y)) 
  }
  
  def sum(self: Rep[OptiMLCompanion], start: Rep[Int], end: Rep[Int], size: Rep[Int], block: Rep[Int => DenseVector[Double]]): Rep[DenseVector[Double]] = {
    try{
      Console.println("catch sum")
      implicit val mf = manifest[Double]
      implicit val cl = OptiMLRunner.vectorCloneable[Double,DenseVector[Double]]
      implicit val ar = OptiMLRunner.denseVectorArith[Double]
      val tpe = TypeRep("generated.scala.DenseVectorDouble")(manifest[DenseVector[Double]])
      val block1 = decompileFun(block)(intType,tpe)
      OptiMLRunner.optiml_sum[DenseVector[Double]](start,end,block1)
    } catch {
      case e => e.printStackTrace; throw e
    }
  }  

  // somehow Delite is generating the entire untilconverged as a singletask, when it should be a DeliteOpWhile that gets unrolled..
  def untilconverged[T](self: Rep[OptiMLCompanion], x: Rep[DenseMatrix[T]], tol: Rep[Double], maxIter: Rep[Int], block: Rep[DenseMatrix[T] => DenseMatrix[T]]): Rep[DenseMatrix[T]] = {
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
      OptiMLRunner.optiml_untilconverged[DenseMatrix[T]](x,(a: Rep[DenseMatrix[T]]) => tol,maxIter,OptiMLRunner.unit(true),block1,diff)
    } catch {
      case e => e.printStackTrace; throw e
    }
  }
  
  def untilconverged2[T](self: Rep[OptiMLCompanion], x: Rep[DenseVector[T]], tol: Rep[Double], maxIter: Rep[Int], block: Rep[DenseVector[T] => DenseVector[T]]): Rep[DenseVector[T]] = {
    try {
      Console.println("catch untilconverged2")
      implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
      implicit val cl = OptiMLRunner.vectorCloneable[Double,DenseVector[Double]].asInstanceOf[OptiMLRunner.Cloneable[DenseVector[T]]]
      implicit val ar = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
      implicit val diff = (a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) => (OptiMLRunner.optila_vector_dist_square(OptiMLRunner.denseVecToInterface(a),OptiMLRunner.denseVecToInterface(b))(mf,ar,implicitly[SourceContext])).asInstanceOf[Rep[Double]]
      // somehow the default string output is just ppl.dsl.optila.DenseVector (without type param)
      val tpe = TypeRep("generated.scala.DenseVectorDouble")(manifest[DenseVector[T]])
      val block1 = decompileFun(block)(tpe,tpe)
      OptiMLRunner.optiml_untilconverged[DenseVector[T]](x,(a: Rep[DenseVector[T]]) => tol,maxIter,OptiMLRunner.unit(true),block1,diff)
    } catch {
      case e => e.printStackTrace; throw e
    }
  }
  
  
}

