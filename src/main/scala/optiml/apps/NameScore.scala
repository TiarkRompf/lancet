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
package apps

import lancet.api._
import lancet.interpreter._
import lancet.core._

import optiml.library._
import optiml.macros.OptiMLLancetRunner._
import Util._

/**
 * Solves problem #22 of Project Euler.
 */
object NameScore {

  def print_usage = {
    println("Usage: NameScore <input data file>")
    exit(-1)
  }
  
  def prog(path: String) = {
    val OptiML = new OptiMLCompanion
    val sc = new ScalaCompanion
    
    val names = sc.stringFromFile(path).split(",")
    
    val namesSanitized = sc.map(names,n => n.substring(1,n.length-1)) // remove quotes
    // sort around 1.5s on names-large.txt
    //OptiML.tic(namesSanitized,namesSanitized)
    val sortedNames = sc.sort(namesSanitized) 
    //OptiML.toc(sortedNames)
    
    OptiML.tic(sortedNames,sortedNames)    
    // Longs seem to be bad news in terms of bytecode complexity
    val scores = sc.map2(sc.zipWithIndex(sortedNames), (t => {
      val a = sc.field1(t) // t._1
      val i = sc.field2(t) // t._2
      // the problematic null check appears to be coming from the bound var c 
      val score = sc.reduce(sc.map3(a,c => c-64),_+_)
      (i*score)//.toLong
    }))

    // compute totalScore
    val z = sc.reduce2(scores,(_+_))
    OptiML.toc(z)
        
    sc.println("total score: ")
    sc.println(z)
    // sc.println("total score: " + sc.longString(z))
    
    42
  }
  
  /**
   * reference version (without hacks):
   */
   /*
   def prog(path: String) = {
     val nameStr = scala.io.Source.fromFile(path).mkString
     val names = nameStr.split(",").map(n => n.slice(1,n.length-1)) // remove quotes
     scala.util.Sorting.quickSort(names) // sort in place

     val scores = names.zipWithIndex map { case (a,i) =>
       val score = a.map(c => c-64).reduce(_+_)
       (i*score).toLong
     }

     // compute totalScore
     scores.reduce(_+_)
   }
  */
  
  def main(args: Array[String]) = {
    if (args.length < 1) print_usage
        
    // macros
    // just crashes somewhere, whether macros are installed or not
    //OptiMLRunner.program = z => prog(args(0))
    //OptiMLRunner.run()
    
    // pure
    //collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(8)
    for (i <- 0 until 10) {
      prog(args(0))
    }
    ()
  }
}
