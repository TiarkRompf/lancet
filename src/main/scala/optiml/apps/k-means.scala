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

object kmeans {

  def print_usage = {
    println("Usage: kmeans <input data file> <initmu data file>")
    exit(-1)
  }
  
  // def prog(x: DenseMatrix[Double], mu: DenseMatrix[Double]) = {
  def prog(xPath: String, muPath: String) = {
    val OptiML = new OptiMLCompanion
    
    val k = 16 // num clusters
    // var iter = 0
    
    val x = OptiML.readMatrix(xPath)
    val mu = OptiML.readMatrix(muPath)
    val m = x.numRows
    
    OptiML.tic(x,mu)
    val newMu = OptiML.untilconverged(mu, .001, 10, { mu: DenseMatrix[Double] =>
      // iter += 1

      val c = OptiML.index_new(0,m).construct{ e => (mu mapRowsToVector { row => OptiML.dist(x.getRow(e), row) }).minIndex }
      val allWP = OptiML.indexvector_hashreduce(OptiML.index_new(0,m), i => c.apply2(i), i => x.getRow(i).Clone2, (a:DenseVector[Double],b:DenseVector[Double]) => a + b)
      val allP = OptiML.indexvector_hashreduce2(OptiML.index_new(0,m), i => c.apply2(i), i => 1, (a:Int,b:Int) => a + b)

      OptiML.index_new(0,k).constructRows { j =>
        val weightedpoints = allWP.apply3(j)
        val points = allP.apply2(j)
        val d = if (points == 0) 1 else points 
        weightedpoints / d
      }
    })    
    OptiML.toc(newMu)
    
    // println("finished in " + iter + " iterations")
    newMu.pprint
    
    42
  }
  
  def main(args: Array[String]) = {
    if (args.length < 2) print_usage
    
    // we'll need to macro these for cluster delite, but we can interpret for cpu/gpu    
    // val OptiML = new OptiMLCompanion
    // val x = OptiML.readMatrix(args(0))
    // val mu = OptiML.readMatrix(args(1))
    
    // macros
    // just crashes somewhere if no macros are installed..
    // OptiMLRunner.program = y => prog(x,mu)
    OptiMLRunner.program = y => prog(args(0),args(1))
    OptiMLRunner.run()
    
    // pure
    // collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(1)
    // for (i <- 0 until 10) {
    //   // prog(x,mu)
    //   prog(args(0),args(1))
    // }
    ()
  }
}
