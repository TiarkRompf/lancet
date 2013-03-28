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
    scala.util.Sorting.quickSort(z)
    z
  }

  def println(x: Any) = Console.println(x.toString)
  
  def longString(x: Int) = x.toString
  
  def field1(t: ((String,Int))) = t._1
  def field2(t: ((String,Int))) = t._2
  
  def zipWithIndex(a: Array[String]): Array[(String,Int)] = a.zipWithIndex
  
  def map(a: Array[String], f: String => String): Array[String] = a.map(f)
  def map2(a: Array[(String,Int)], f: ((String,Int)) => Int): Array[Int] = a.map(f)
  def map3(a: String, f: Char => Int): Array[Int] = a.map(f).toArray
  
  def reduce(a: Array[Int], f: (Int,Int) => Int): Int = a.reduce(f)
  def reduce2(a: Array[Int], f: (Int,Int) => Int): Int = a.reduce(f)
}