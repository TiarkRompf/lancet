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
package library

import java.io.{BufferedReader,FileReader}

import lancet.api._
import lancet.interpreter._
import lancet.core._

class ScalaCompanion {

  def stringFromFile(path: String) = scala.io.Source.fromFile(path).mkString
  
  def sort(x: Array[String]) = {
    val z = new Array[String](x.length)
    System.arraycopy(x,0,z,0,x.length)
    //java.util.Arrays.sort(z.asInstanceOf[Array[Object]])
    scala.util.Sorting.quickSort(z)
    z
  }

  def println(x: Any) = Console.println(x.toString)
  
  def longString(x: Int) = x.toString
  
  def field1(t: ((String,Int))) = t._1
  def field2(t: ((String,Int))) = t._2
  
  def zipWithIndex(a: Array[String]): Array[(String,Int)] = a.zipWithIndex
  
  def map(a: Array[String], f: String => String): Array[String] = a.map(f)
  def map2(a: Array[(String,Int)], f: ((String,Int)) => Int): Array[Int] = {
    val out = new Array[Int](a.length)
    for (i <- (0 until a.length).par) out(i) = f(a(i))
    out
    //a.par.map(f).seq
  }
  def map3(a: String, f: Char => Int): Array[Int] = a.map(f).toArray
  
  def reduce(a: Array[Int], f: (Int,Int) => Int): Int = a.reduce(f)
  def reduce2(a: Array[Int], f: (Int,Int) => Int): Int = a.par.reduce(f)
}
