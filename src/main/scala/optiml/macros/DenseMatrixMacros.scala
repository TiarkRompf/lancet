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

//import ppl.dsl.optila.{Vector,DenseVector,DenseVectorView,DenseMatrix,RangeVector}
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

  def numCols[T](self: Rep[DenseMatrix[T]]): Rep[Int] = {
    Console.println("catch matrix_numcols")
    implicit val mf = manifest[Double].asInstanceOf[Manifest[T]] //FIXME: generic types
    OptiMLRunner.densematrix_numcols(self)
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

