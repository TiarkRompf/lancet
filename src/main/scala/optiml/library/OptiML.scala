package optiml
package library

import java.io.{BufferedReader,FileReader}

import lancet.api._
import lancet.interpreter._
import lancet.core._

/**
 * Library implementation of OptiML language ops
 * 
 * We currently do not handle generic types (we assume Double everywhere).
 * 
 * This whole file is a hacky mess. We should implement it using proper
 * generics (or Forge) if we are doing it for real. The current version
 * uses no generics (either inheritance or type classes) and explicitly 
 * avoids overloading, just to keep things as simple as possible.
 */
 
// object OptiML {
class OptiMLCompanion {
  val DenseVector = new DenseVectorCompanion
  val DenseMatrix = new DenseMatrixCompanion

  /**
   * i/o
   */ 
  def readVector(path: String): DenseVector[Double] = {
    val x = new DenseVector[Double](0, true)
    val xfs = new BufferedReader(new FileReader(path))
    var line = xfs.readLine()    
    while (line != null){      
      line = line.trim()
      x <<= line.toDouble
      line = xfs.readLine()
    }
    xfs.close()    
    x
  }
  
  def readMatrix(path: String): DenseMatrix[Double] = {
    val delim = "\\s+"
    
    val xfs = new BufferedReader(new FileReader(path))
    var line = xfs.readLine()
    line = line.trim()
    var elems = line.split(delim)
    val x = new DenseMatrix[Double](0, elems.length)  
    
    while (line != null){
      val v = index_new(0,elems.length) map { i => elems(i).toDouble } 
      x <<= v  
      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        elems = line.split(delim)
      }
    }
    xfs.close()
    x
  }
  
  /**
   * index vector
   */
   // implicit def intToIndexOp(i: Int): IndexOp = new IndexOp(i)
   // 
   // class IndexOp(val _end: Int) {
   //   def ::(_start: Int): IndexVector = new IndexVectorRange(_start,_end)
   // }  
   def index_new(start: Int, end: Int): IndexVectorRange = new IndexVectorRange(start,end)
   def indexvector_hashreduce(x: IndexVectorRange, f: Int => Int, map: Int => DenseVector[Double], reduce: (DenseVector[Double],DenseVector[Double]) => DenseVector[Double]): DenseVector[DenseVector[Double]] = {
     val out = new DenseVector[DenseVector[Double]](x._end-x._start,true) // every index is a group
     for (i <- x._start until x._end) {
       val grp = f(i)
       val elem = map(i)
       if (out(grp) == null) out(grp) = elem
       else out(grp) = reduce(out(grp),elem)
     }
     out
   }
   
   def indexvector_hashreduce2(x: IndexVectorRange, f: Int => Int, map: Int => Int, reduce: (Int,Int) => Int): DenseVector[Int] = {
     val out = new DenseVector[Int](x._end-x._start,true) // every index is a group
     for (i <- x._start until x._end) {
       val grp = f(i)
       val elem = map(i)
       out(grp) = reduce(out(grp),elem)
     }
     out
   }
   
   
   /**
    * untilconverged
    */
  
   def untilconverged[T](x: DenseMatrix[T], tol: Double, maxIter: Int, block: DenseMatrix[T] => DenseMatrix[T]): DenseMatrix[T] = {
     var iter = 0
     var last = x
     var converged = false
     while (iter < maxIter && !converged) {
       val next = block(last)
       if (dist3(next,last).asInstanceOf[Double] < tol) {  // hack
         converged = true
       }
       last = next
       iter += 1
     }
     last
   }
   
   def untilconverged2[T](x: DenseVector[T], tol: Double, maxIter: Int, block: DenseVector[T] => DenseVector[T]): DenseVector[T] = {
     var iter = 0
     var last = x
     var converged = false
     while (iter < maxIter && !converged) {
       val next = block(last)
       if (dist2(next,last).asInstanceOf[Double] < tol) {  // hack
         converged = true
       }
       last = next
       iter += 1
     }
     last     
   }
   
   /**
    * sum
    */
  def sum(start: Int, end: Int, size: Int, block: Int => DenseVector[Double]) = {
    val acc = new DenseVector[Double](size, true)
    for (i <- start until end) {
      acc += block(i)
    }
    acc
  }
      
  /**
   * dist
   */
   def dist[T](a: DenseVectorView[T], b: DenseVectorView[T]): T = {
     // hack: assume T is double
     // use sum of squares
     var ans = 0.0
     for (i <- 0 until a._length) {
       ans += (a.apply4(i).asInstanceOf[Double]-b.apply4(i).asInstanceOf[Double])*(a.apply4(i).asInstanceOf[Double]-b.apply4(i).asInstanceOf[Double])
     }
     ans.asInstanceOf[T]
   }

   def dist2[T](a: DenseVector[T], b: DenseVector[T]): T = {
     // hack: assume T is double
     // use sum of squares
     var ans = 0.0
     for (i <- 0 until a.length) {
       ans += (a(i).asInstanceOf[Double]-b(i).asInstanceOf[Double])*(a(i).asInstanceOf[Double]-b(i).asInstanceOf[Double])
     }
     ans.asInstanceOf[T]
   }
   
   def dist3[T](a: DenseMatrix[T], b: DenseMatrix[T]): T = {
     // hack: assume T is double
     // use sum of squares
     var ans = 0.0
     for (i <- 0 until a.numRows) {
       for (j <- 0 until a.numCols) {
         ans += (a(i,j).asInstanceOf[Double]-b(i,j).asInstanceOf[Double])*(a(i,j).asInstanceOf[Double]-b(i,j).asInstanceOf[Double])
       }       
     }
     ans.asInstanceOf[T]
   }   
   
   /**
    * profiling
    */
  def tic[T](args: T*): Unit = ???
  def toc[T](args: T*): Unit = ???
}