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
package lancet
package advanced

import lancet.api._
import lancet.interpreter._
import lancet.core._

//import ppl.dsl.optila.{Vector,DenseVector,RangeVector}
//import ppl.dsl.optiml.{IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config

import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}
import scala.virtualization.lms.internal.{GenericFatCodegen}

import scala.virtualization.lms.common._

class TestDelite2 extends FileDiffSuite {
  
  val prefix = "test-out/test-delite-2"

  def testA1 = withOutFileChecked(prefix+"A1") {
    import DeliteRunner._

    /* 
    CURRENT STATUS / STEPS:
      - work around issues/conflicts: IfThenElse, compile{x=>}
      - using lancet lms codegen with embedded delite objects
      - make delite scala codegen generate lancet stuff
      - use delite backend codegen. lancet just for decompiling bytecode

    TODO: 
      - need to hook into DeliteCodegen for Base_LMS classes?
      - ISSUE: control flow?? need to use DeliteIf, and don't have functions ...
      - lancet generates lots of strong effect dependencies (objects, fields, etc): remove!!
      - what to do about generic types? require manifests and evaluate then at compile time?
    */

    def printxx(x:Any) = { println(x) }

    class VectorCompanion {
      def rand(n:Int): Vector[Double] = { printxx("Vector$.rand"); new Vector[Double] }
      def apply[T:Manifest](xs: T*): Vector[T] = { printxx("Vector$.apply"); new Vector[T] }
    }
    class Vector[T] {
      def t: Vector[T] = { printxx("Vector.t"); this } //new Vector[T] }  FIXME: ISSUE WITH OUTER POINTER / Delite conditionals
      def isRow: Boolean = { printxx("Vector.isRow"); false }
      def sum: T = ???
      def max: T = ???
      def min: T = ???
      def length: Int = ???
    }

    class UtilCompanion {
      def mean(v: Vector[Int]): Int = v.sum / v.length
      def max(v: Vector[Int]): Int = v.max
      def min(v: Vector[Int]): Int = v.min
      def collect(b: Boolean): Unit = { println(b) }
    }

    val Vector = new VectorCompanion
    val Util = new UtilCompanion

    def myprog = {
      import Util._

      val v = Vector.rand(1000)

      val vt = v.t
      //collect(vt.isRow != v.isRow)

      //val vc = v.clone
      //collect(vc.cmp(v) == true)

      val v2 = Vector(1,2,3,4,5)
      //collect(median(v2) == 3)
      collect(mean(v2) == 3)
      collect(max(v2) == 5)
      collect(min(v2) == 1)

      printxx("AA")
      printxx("BB")
      printxx("CC")

      42 // need result?
    }

    val VectorOperatorsRunner = new LancetDeliteRunner

    VectorOperatorsRunner.initialize()
    VectorOperatorsRunner.traceMethods = false
    VectorOperatorsRunner.emitUniqueOpt = true
    
    object Macros extends VectorOperatorsRunner.ClassMacros {
      val targets = List(classOf[VectorCompanion],classOf[Vector[_]])
      //type Rep[T] = VectorOperatorsRunner.Rep[T]
      import VectorOperatorsRunner._ //{Rep,reflect,mtr,infix_relax,eval,VConst,Def}
      def rand(self: Rep[VectorCompanion], n: Rep[Int]): Rep[DenseVector[Double]] = {
        Console.println("catch vector_rand")
        VectorOperatorsRunner.densevector_obj_rand(n)
      }
      def apply[T](self: Rep[VectorCompanion], xs: Rep[Seq[T]], mf: Rep[Manifest[T]]): Rep[DenseVector[T]] = {
        Console.println("catch vector_apply")
        implicit val mfT = eval(mf) match {
          case VConst(mf: Manifest[T]) => mf
          case _ => 
            //Console.println("ERROR: non-constant manifest in vector_apply: "+mf+"="+Def.unapply(mf)+" -- assuming Double")
            Console.println("ERROR: non-constant manifest in vector_apply -- assuming Double")
            manifest[Double].asInstanceOf[Manifest[T]]
        }

        //implicit val mf = manifest[Int].asInstanceOf[Manifest[T]] //FIXME: generic types
        val xs1 = reflect[Seq[T]](xs,".asInstanceOf[Seq[Int]]")(mtr[Seq[Int]].relax) // need cast ...
        VectorOperatorsRunner.densevector_obj_fromseq(xs1)
        // TODO: generic types are problematic...
        // require manifest parameter and try to eval that?
        // or use scala reflection?
      }
      def t[T](self: Rep[DenseVector[T]]): Rep[DenseVector[T]] = {
        Console.println("catch vector_t")
        implicit val mf = manifest[Int].asInstanceOf[Manifest[T]] //FIXME: generic types
        VectorOperatorsRunner.densevector_trans(self)
      }
      def isRow[T](self: Rep[DenseVector[T]]): Rep[Boolean] = {
        Console.println("catch vector_isRow")
        implicit val mf = manifest[Int].asInstanceOf[Manifest[T]] //FIXME: generic types
        VectorOperatorsRunner.densevector_isrow(self)
      }
      def length[T](self: Rep[DenseVector[T]]): Rep[Int] = {
        Console.println("catch vector_length")
        implicit val mf = manifest[Int].asInstanceOf[Manifest[T]] //FIXME: generic types
        VectorOperatorsRunner.densevector_length(self)
      }
      def sum[T](self: Rep[DenseVector[T]]): Rep[T] = {
        Console.println("catch vector_sum")
        implicit val mf = manifest[Int].asInstanceOf[Manifest[T]] //FIXME: generic types
        implicit val af = VectorOperatorsRunner.intArith.asInstanceOf[VectorOperatorsRunner.Arith[T]] //FIXME: generic types
        VectorOperatorsRunner.vector_sum(VectorOperatorsRunner.denseVecToInterface(self))
      }
      def max[T](self: Rep[DenseVector[T]]): Rep[T] = {
        Console.println("catch vector_max")
        implicit val mf = manifest[Int].asInstanceOf[Manifest[T]] //FIXME: generic types
        implicit val af = implicitly[Ordering[Int]].asInstanceOf[Ordering[T]] //FIXME: generic types
        implicit val bf = VectorOperatorsRunner.intHasMinMax.asInstanceOf[VectorOperatorsRunner.HasMinMax[T]] //FIXME: generic types
        VectorOperatorsRunner.vector_max(VectorOperatorsRunner.denseVecToInterface(self))
      }
      def min[T](self: Rep[DenseVector[T]]): Rep[T] = {
        Console.println("catch vector_min")
        implicit val mf = manifest[Int].asInstanceOf[Manifest[T]] //FIXME: generic types
        implicit val af = implicitly[Ordering[Int]].asInstanceOf[Ordering[T]] //FIXME: generic types
        implicit val bf = VectorOperatorsRunner.intHasMinMax.asInstanceOf[VectorOperatorsRunner.HasMinMax[T]] //FIXME: generic types
        VectorOperatorsRunner.vector_min(VectorOperatorsRunner.denseVecToInterface(self))
      }
    }

    VectorOperatorsRunner.install(Macros)

    VectorOperatorsRunner.program = x => myprog

    // now run stuff....

    VectorOperatorsRunner.VConstantPool = scala.collection.immutable.Vector.empty

    VectorOperatorsRunner.generateScalaSource("Generated", new java.io.PrintWriter(System.out))

    VectorOperatorsRunner.globalDefs.foreach(println)    

    val cst = VectorOperatorsRunner.VConstantPool
    println("constants: "+cst)

    //println("*** running execute ***")

    //VectorOperatorsRunner.execute(Array())

    println("*** running compileAndTest ***")
    
    compileAndTest(VectorOperatorsRunner)

  }
}