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


