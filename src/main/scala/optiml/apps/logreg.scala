package optiml
package apps

import lancet.api._
import lancet.interpreter._
import lancet.core._

import optiml.library._
import optiml.macros.OptiMLLancetRunner._
import Util._

object logreg {

  def print_usage = {
    println("Usage: logreg <input data file> <input label file>")
    exit(-1)
  }
  
  // def prog(x: DenseMatrix[Double], y: DenseVector[Double]) = {
  def prog(xPath: String, yPath: String) = {
    val OptiML = new OptiMLCompanion

    val x = OptiML.readMatrix(xPath)
    val y = OptiML.readVector(yPath).t

    OptiML.tic(x,y)
    
    val theta = OptiML.DenseVector.zeros(x.numCols,true)//new DenseVector[Double](x.numCols,true)
    var iter = 0
        
    val w = OptiML.untilconverged2(theta, .001, 30, { cur: DenseVector[Double] => 
      // println("current theta: ")
      // cur.pprint
       
      val gradient = OptiML.index_new(0, x.numRows).construct2 { i: Int =>
        (x.getRow(i).times2(y(i) - (1.0 / (1.0 + OptiML.exp(cur*(-1.0) *:* x.getRow(i)))))).Clone
      }.sum       
      
      iter += 1
      cur + gradient // note that each term in the gradient has opposite sign as in Spark      
    })
    
    OptiML.toc(w)
    println("w:")
    w.pprint   
    
    42     
  }
  
  def main(args: Array[String]) = {
    if (args.length < 2) print_usage
        
    // we'll need to macro these for cluster delite, but we can interpret for cpu/gpu    
    // val OptiML = new OptiMLCompanion
    // val x = OptiML.readMatrix(args(0))
    // val y = OptiML.readVector(args(1)).t
    
    // macros
    // just crashes somewhere, whether macros are installed or not
    // OptiMLRunner.program = z => prog(x,y)
    //OptiMLRunner.program = z => prog(args(0),args(1))
    // OptiMLRunner.run()
    
    // pure
    //collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(1)
    for (i <- 0 until 10) {
       // prog(x,y)
       prog(args(0),args(1))
     }
    ()
  }
}
