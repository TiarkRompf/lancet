package optiml
package macros

import lancet.api._
import lancet.interpreter._
import lancet.core._

import optiml._

object OptiMLLancetRunner {
  val OptiMLRunner = new LancetDeliteRunner

  OptiMLRunner.initialize()
  OptiMLRunner.traceMethods = false
  OptiMLRunner.emitUniqueOpt = true
  
  // add macros
  OptiMLRunner.install(OptiMLMacros)  
  OptiMLRunner.install(DenseVectorMacros)
  OptiMLRunner.install(DenseMatrixMacros)
  OptiMLRunner.install(IndexVectorMacros)
  
  def infix_run(runner: LancetDeliteRunner) = {
    // now run stuff....
    println("starting run")
    
    runner.VConstantPool = scala.collection.immutable.Vector.empty    
    runner.generateScalaSource("Generated", new java.io.PrintWriter(System.out))
    runner.globalDefs.foreach(println)    

    val cst = runner.VConstantPool
    println("constants: "+cst)

    //println("*** running execute ***")

    //runner.execute(Array())

    println("*** running compileAndTest ***")
    
    DeliteRunner.compileAndTest(runner)        
  }  
}
