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

import scala.collection.mutable.ArrayOps

import lancet.api._
import lancet.interpreter._
import lancet.core._

import scala.virtualization.lms.internal.{GenericFatCodegen}
import scala.virtualization.lms.common._

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config
import ppl.delite.framework.datastructures.DeliteArray

import optiml.library.ScalaCompanion
import OptiMLLancetRunner._

// TODO: get this stuff working with ArrayOps, StringOps. The only thing that seems to
// be missing is being able to recover the Rep[Array[_]] instance from a Rep[ArrayOps[_]].

object BenchmarkAccelerationMacros extends OptiMLRunner.ClassMacros {
  val targets = List(classOf[ScalaCompanion],classOf[Char],classOf[Int],classOf[String],classOf[Array[_]])
  import OptiMLRunner._ //{Rep,reflect,mtr,infix_relax,decompileFun}
  
  def longString(self: Rep[ScalaCompanion], l: Rep[Int]): Rep[String] = {
    Console.println("catch long_tostring")
    OptiMLRunner.object_tostring(l)
  }
  
  def println(self: Rep[ScalaCompanion], x: Exp[Any]): Rep[Unit] = {
    Console.println("catch println")
    OptiMLRunner.println(ensureType(x))    
  }
  
  def stringFromFile(self: Rep[ScalaCompanion], path: Rep[String]): Rep[String] = {
    Console.println("catch string_from_file")    
    OptiMLRunner.source_stringfromfile(ensureType(path))
  }  
  
  def field1(self: Rep[ScalaCompanion], x: Rep[(String,Int)]): Rep[String] = {
    Console.println("catch field1")
    OptiMLRunner.tuple2_get1(x)
  }

  def field2(self: Rep[ScalaCompanion], x: Rep[(String,Int)]): Rep[Int] = {
    Console.println("catch field2")
    OptiMLRunner.tuple2_get2(x)
  }
  
  def sort(self: Rep[ScalaCompanion], x: Rep[DeliteArray[String]]): Rep[DeliteArray[String]] = {
    Console.println("catch quicksort")    
    OptiMLRunner.darray_sort(x)
  }  
  
  def split(self: Rep[String], del: Rep[String]): Rep[DeliteArray[String]] = {
    Console.println("catch string_split")    
    OptiMLRunner.darray_fromsplit(self,del)
  }  

  def length(self: Rep[String]): Rep[Int] = {
    Console.println("catch string_length")    
    OptiMLRunner.string_length(self)
  }  
  
  def substring(self: Rep[String], start: Rep[Int], end: Rep[Int]): Rep[String] = {
    Console.println("catch string_slice")    
    OptiMLRunner.string_slice(self,start,end)
  }  
  
  def zipWithIndex(self: Rep[ScalaCompanion], a: Rep[DeliteArray[String]]): Rep[DeliteArray[(String,Int)]] = {
    Console.println("catch array_zipwithindex")    
    OptiMLRunner.darray_zipwithindex(a)
  }  
  
  def map(self: Rep[ScalaCompanion], a: Rep[DeliteArray[String]], f: Rep[String => String]): Rep[DeliteArray[String]] = {
    Console.println("catch array_map")    
    val f1 = decompileFun(f)(stringType,stringType)    
    OptiMLRunner.darray_map(a,f1)
  }  
  
  def map2(self: Rep[ScalaCompanion], a: Rep[DeliteArray[(String,Int)]], f: Rep[((String,Int)) => Int]): Rep[DeliteArray[Int]] = {
    Console.println("catch array_map2")    
    val argType = TypeRep("generated.scala.DeliteArray[(String,Int)]")(manifest[(String,Int)])
    val f1 = decompileFun(f)(argType,intType)    
    OptiMLRunner.darray_map(a,f1)
  }  
  
  def map3(self: Rep[ScalaCompanion], s: Rep[String], f: Rep[Char => Int]): Rep[DeliteArray[Int]] = {
    Console.println("catch string_map")    
    val f1 = decompileFun(f)(charType,intType)    
    OptiMLRunner.string_map(s,f1)
  }    
  
  def reduce(self: Rep[ScalaCompanion], a: Rep[DeliteArray[Int]], f: Rep[(Int,Int) => Int]): Rep[Int] = {
    Console.println("catch array_reduce")    
    val f1 = decompileFun2(f)(intType,intType,intType)
    OptiMLRunner.darray_reduce(a,f1,OptiMLRunner.unit(0))
  }    
  
  def reduce2(self: Rep[ScalaCompanion], a: Rep[DeliteArray[Int]], f: Rep[(Int,Int) => Int]): Rep[Int] = {
    Console.println("catch array_reduce2")    
    val f1 = decompileFun2(f)(intType,intType,intType)
    OptiMLRunner.darray_reduce(a,f1,OptiMLRunner.unit(0))
  }      

}


