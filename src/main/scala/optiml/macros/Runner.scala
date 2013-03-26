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
  OptiMLRunner.traceMethods = false
  OptiMLRunner.emitUniqueOpt = true
  OptiMLRunner.emitCheckCast = false
  
  // add macros
  OptiMLRunner.install(OptiMLMacros)  
  OptiMLRunner.install(DenseVectorMacros)
  OptiMLRunner.install(DenseMatrixMacros)
  OptiMLRunner.install(IndexVectorMacros)
  
  def infix_run(runner: LancetDeliteRunner) = {
    // now run stuff....
    println("starting run")
    
    runner.VConstantPool = scala.collection.immutable.Vector.empty    
    // Config.cacheSyms = false
    // runner.generateScalaSource("Generated", new java.io.PrintWriter(System.out))
    // runner.globalDefs.foreach(println)    

    val cst = runner.VConstantPool
    println("constants: "+cst)

    //println("*** running execute ***")

    //runner.execute(Array())

    println("*** running compileAndTest ***")
    
    DeliteRunner.compileAndTest(runner)        
  }  
}
