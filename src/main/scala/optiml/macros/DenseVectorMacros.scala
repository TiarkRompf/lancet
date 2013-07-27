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

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config

//import ppl.dsl.optila.{Vector,DenseVector,DenseVectorView,RangeVector}
//import ppl.dsl.optiml.{IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}

import optiml.library.DenseVectorCompanion
import OptiMLLancetRunner._

object DenseVectorMacros extends OptiMLRunner.ClassMacros {
  val targets = List(classOf[optiml.library.DenseVectorCompanion],classOf[optiml.library.DenseVector[_]],classOf[optiml.library.DenseVectorView[_]])
  //type Rep[T] = VectorOperatorsRunner.Rep[T]
  import OptiMLRunner._//{Rep,reflect,mtr,infix_relax}
  
  def rand(self: Rep[DenseVectorCompanion], n: Rep[Int]): Rep[DenseVector[Double]] = {
    Console.println("catch vector_rand")
    OptiMLRunner.densevector_obj_rand(n)
  }
  def zeros(self: Rep[DenseVectorCompanion], n: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[Double]] = {
    Console.println("catch vector_new")
    OptiMLRunner.densevector_obj_zeros(n)
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
  def apply[T](self: Rep[DenseVector[T]], n: Rep[Int]): Rep[T] = {
    Console.println("catch vector_apply1")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]]
    OptiMLRunner.densevector_apply(self,n)
  }  
  def apply2[T](self: Rep[DenseVector[T]], n: Rep[Int]): Rep[T] = {
    Console.println("catch vector_apply2")
    implicit val mf = manifest[Int].asInstanceOf[Manifest[T]]
    OptiMLRunner.densevector_apply(self,n)
  }
  def apply3[T](self: Rep[DenseVector[T]], n: Rep[Int]): Rep[T] = {
    Console.println("catch vector_apply3")
    implicit val mf = manifest[DenseVector[Double]].asInstanceOf[Manifest[T]]
    OptiMLRunner.densevector_apply(self,n)
  }  
  def apply4[T](self: Rep[DenseVectorView[T]], n: Rep[Int]): Rep[T] = {
    Console.println("catch vector_view_apply")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]]
    OptiMLRunner.dense_vectorview_apply(self,n)
  }  
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
  def sum[T](self: Rep[DenseVector[T]]): Rep[T] = {
    Console.println("catch vector_sum")
    implicit val mf = manifest[DenseVector[Double]].asInstanceOf[Manifest[T]] //FIXME: generic types
    //implicit val a = OptiMLRunner.doubleArith
    implicit val ar = OptiMLRunner.denseVectorArith[Double].asInstanceOf[OptiMLRunner.Arith[T]]
    OptiMLRunner.vector_sum(OptiMLRunner.denseVecToInterface(self))
  }      
  def minIndex[T](self: Rep[DenseVector[T]]): Rep[Int] = {
    Console.println("catch vector_minIndex")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val af = implicitly[Ordering[Double]].asInstanceOf[Ordering[T]] //FIXME: generic types
    implicit val bf = OptiMLRunner.doubleHasMinMax.asInstanceOf[OptiMLRunner.HasMinMax[T]] //FIXME: generic types    
    OptiMLRunner.vector_minindex(OptiMLRunner.denseVecToInterface(self))
  }    
  def Clone[T](self: Rep[DenseVector[T]]): Rep[DenseVector[T]] = {
    Console.println("catch vector_clone")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_clone[T,DenseVector[T]](OptiMLRunner.denseVecToInterface(self))
  }
  def Clone2[T](self: Rep[DenseVectorView[T]]): Rep[DenseVector[T]] = {
    Console.println("catch vectorview_clone")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_clone[T,DenseVector[T]](OptiMLRunner.denseViewToInterface(self))
  }  
  def *:*[T](self: Rep[DenseVector[T]], b: Rep[DenseVectorView[T]]): Rep[T] = {    
    Console.println("catch vector_dot_product")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    OptiMLRunner.vector_dot_product[T](OptiMLRunner.denseVecToInterface(self),OptiMLRunner.denseViewToInterface(b))
  }  
  def /[T](self: Rep[DenseVector[T]], b: Rep[T]): Rep[DenseVector[T]] = {    
    Console.println("catch vector_divide_scalar")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_divide_scalar[T,DenseVector[T]](OptiMLRunner.denseVecToInterface(self),b)
  }
  def -[T](self: Rep[DenseVector[T]], b: Rep[T]): Rep[DenseVector[T]] = {    
    Console.println("catch vector_minus_scalar")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_minus_scalar[T,DenseVector[T]](OptiMLRunner.denseVecToInterface(self),b)
  }  
  def times2[T](self: Rep[DenseVectorView[T]], b: Rep[T]): Rep[DenseVector[T]] = {    
    Console.println("catch vectorview_times_scalar")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_times_scalar[T,DenseVector[T]](OptiMLRunner.denseViewToInterface(self),b)
  }    
  def *[T](self: Rep[DenseVector[T]], b: Rep[T]): Rep[DenseVector[T]] = {    
    Console.println("catch vector_times_scalar")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_times_scalar[T,DenseVector[T]](OptiMLRunner.denseVecToInterface(self),b)
  }      
  def +[T](self: Rep[DenseVector[T]], b: Rep[DenseVector[T]]): Rep[DenseVector[T]] = {    
    Console.println("catch vector_plus")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    implicit val a = OptiMLRunner.doubleArith.asInstanceOf[OptiMLRunner.Arith[T]]
    implicit val bldr = OptiMLRunner.denseVectorBuilder[Double].asInstanceOf[OptiMLRunner.VectorBuilder[T,DenseVector[T]]]
    OptiMLRunner.vector_plus[T,DenseVector[T]](OptiMLRunner.denseVecToInterface(self),OptiMLRunner.denseVecToInterface(b))
  }
  def pprint[T](self: Rep[DenseVector[T]]): Rep[Unit] = {
    Console.println("catch vector_pprint")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.vector_pprint(OptiMLRunner.denseVecToInterface(self))
  }    
}
