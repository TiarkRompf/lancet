package optiml
package apps

import lancet.api._
import lancet.interpreter._
import lancet.core._

import optiml.library._
import optiml.macros.OptiMLLancetRunner._
import Util._

object kmeans {

  // vars end up as objects..
  
  // var xPath: String = _
  // var muPath: String = _
  val xPath = "/Users/asujeeth/data/ml/kmeans/mandrill-large.dat"
  val muPath = "/Users/asujeeth/data/ml/kmeans/initmu.dat"
  
  def print_usage = {
    println("Usage: kmeans <input data file> <initmu data file>")
    exit(-1)
  }
  
  def prog = {
    val OptiML = new OptiMLCompanion
    
    val tol = 0.001 // tolerance (for convergence)
    val k = 16 // num clusters
    // var iter = 0
    
    val x = OptiML.readMatrix(xPath)
    val mu = OptiML.readMatrix(muPath)
    val m = x.numRows
    
    // OptiML.tic(mu)
    val newMu = OptiML.untilconverged(mu, tol, { mu: DenseMatrix[Double] =>
      // iter += 1

      val c = OptiML.index_new(0,m).construct{ e => (mu mapRowsToVector { row => OptiML.dist(x(e), row) }).minIndex }
      val allWP = OptiML.indexvector_hashreduce(OptiML.index_new(0,m), i => c(i), i => x(i).Clone, (a:DenseVector[Double],b:DenseVector[Double]) => a + b)
      val allP = OptiML.indexvector_hashreduce(OptiML.index_new(0,m), i => c(i), i => 1, (a:Int,b:Int) => a + b)

      OptiML.index_new(0,k).constructRows { j =>
        val weightedpoints = allWP(j)
        val points = allP(j)
        val d = if (points == 0) 1 else points 
        weightedpoints / d
      }
    })    
    // OptiML.toc(newMu)
    
    // println("finished in " + iter + " iterations")
    newMu.pprint
    
    42
  }
  
  def simple = {
    val DenseVector = new DenseVectorCompanion
    
    val v = DenseVector.rand(1000)

    val vt = v.t
    //collect(vt.isRow != v.isRow)

    //val vc = v.clone
    //collect(vc.cmp(v) == true)

    // val v2 = DenseVector(1,2,3,4,5)
    //collect(median(v2) == 3)
    // collect(mean(v2) == 3)
    // collect(max(v2) == 5)
    // collect(min(v2) == 1)
     
    printxx("AA")
    printxx("BB")
    printxx("CC")

    42 // need result?
  }
  
  def main(args: Array[String]) = {
    // if (args.length < 1) print_usage
    // xPath = args(0)
    // muPath = args(1)    
    OptiMLRunner.program = x => prog
    OptiMLRunner.run()
  }
}