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

import ppl.delite.framework.Config

import optiml._

object OptiMLLancetRunner {
  val OptiMLRunner = new LancetDeliteRunner

  OptiMLRunner.initialize()
  OptiMLRunner.traceMethods = true
  OptiMLRunner.emitUniqueOpt = true
  OptiMLRunner.emitCheckCast = false
  
  // add macros
  OptiMLRunner.install(OptiMLMacros)  
  OptiMLRunner.install(DenseVectorMacros)
  OptiMLRunner.install(DenseMatrixMacros)
  OptiMLRunner.install(IndexVectorMacros)
  OptiMLRunner.install(BenchmarkAccelerationMacros)
  
  def infix_run(runner: LancetDeliteRunner) = {
    // now run stuff....
    println("starting run")
    
    runner.VConstantPool = scala.collection.immutable.Vector.empty    
    // Config.cacheSyms = false
    // runner.generateScalaSource("Generated", new java.io.PrintWriter(System.out))
    // runner.globalDefs.foreach(println)    
    // sys.exit(0)
    val cst = runner.VConstantPool
    println("constants: "+cst)

    //println("*** running execute ***")

    //runner.execute(Array())

    println("*** running compileAndTest ***")
    
    DeliteRunner.verboseDefs = true
    DeliteRunner.compileAndTest(runner)        
  }  
}
