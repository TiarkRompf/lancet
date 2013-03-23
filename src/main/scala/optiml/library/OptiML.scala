package optiml
package library

import java.io.{BufferedReader,FileReader}

import lancet.api._
import lancet.interpreter._
import lancet.core._

/**
 * Library implementation of OptiML language ops
 * 
 * We currently do not handle generic types (we assume Double everywhere)
 */
// object OptiML {
class OptiMLCompanion {
  /**
   * i/o
   */ 
  def readMatrix(path: String): DenseMatrix[Double] = {
    ???
    
    /*
    val delim = "\\s+"
    
    val xfs = new BufferedReader(new FileReader(path))
    var line = xfs.readLine()
    line = line.trim()
    var elems = line.split(delim)
    val x = new DenseMatrix[Double](0, elems.length)  
    
    while (line != null){
      val v = (0::elems.length) map { i => i.toDouble } 
      x <<= v  
      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        elems = line.split(delim)
      }
    }
    xfs.close()
    x
    */
  }
  
  /**
   * index vector
   */
   // implicit def intToIndexOp(i: Int): IndexOp = new IndexOp(i)
   // 
   // class IndexOp(val _end: Int) {
   //   def ::(_start: Int): IndexVector = new IndexVectorRange(_start,_end)
   // }  
   def index_new(start: Int, end: Int): IndexVectorRange = ???
   def indexvector_hashreduce[V](x: IndexVectorRange, f: Int => Int, map: Int => V, reduce: (V,V) => V): DenseVector[V] = ???   
   
   /**
    * untilconverged
    */
   def untilconverged[T](x: DenseMatrix[T], tol: Double, block: DenseMatrix[T] => DenseMatrix[T]): DenseMatrix[T] = ???
   
      
  /**
   * dist
   */
   def dist[T](a: DenseVectorView[T], b: DenseVector[T]): T = ???
   
   
   /**
    * profiling
    */
  def tic[T](args: T*): Unit = ???
  def toc[T](args: T*): Unit = ???
}