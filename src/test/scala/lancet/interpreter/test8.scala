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
package interpreter

import lancet.api._

class TestInterpreter8 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-8"

  import java.math.BigInteger




  def test1 = withOutFileChecked(prefix+"bigint1") {
    val it = new Compiler

    abstract class SafeInt { def toBigInt: BigInteger }
    case class Small(x: Int) extends SafeInt { def this() = this(0); def toBigInt = BigInteger.valueOf(x) }
    case class Big(x: BigInteger) extends SafeInt { def toBigInt = x }

    def SafeInt(x: Long): SafeInt = 
      if (x >= Int.MinValue && x <= Int.MaxValue) Small(x.toInt)
      else { it.slowpath(); Big(BigInteger.valueOf(x)) }

    /*def infix_*(x: SafeInt, y: SafeInt): SafeInt = (x,y) match {
      case (Small(x),Small(y)) => SafeInt(x.toLong * y.toLong)
      case _ => it.slowpath2(); Big(x.toBigInt.multiply(y.toBigInt))
    }*/
    def infix_**(x: SafeInt, y: SafeInt): SafeInt = 
    if (x.isInstanceOf[Small] && y.isInstanceOf[Small]) {
      val x1 = x.asInstanceOf[Small].x
      val y1 = y.asInstanceOf[Small].x
      SafeInt(x1.toLong * y1.toLong)
    } else {
      it.slowpath(); Big(x.toBigInt.multiply(y.toBigInt))
    }


    def fac(n: Int) = {
      var i = 1
      var prod = SafeInt(1)
      while (i < n) {
        prod = prod ** SafeInt(i)
        i += 1
      }
      prod.asInstanceOf[Small].x
    }

    def fac2(n: Int) = {
      var i = 1
      var prod = 1
      while (i < n) {
        prod = prod * i
        i += 1
      }
      prod
    }


    val f = it.compile { (n:Int) => 
      //val m = fac(x)
      val m = {
        var i = 1
        var prod = SafeInt(1)
        while (i < n) {
          val x = prod
          val y = SafeInt(i)
          prod = {
            if (x.isInstanceOf[Small] && y.isInstanceOf[Small]) {
              val x1 = x.asInstanceOf[Small].x
              val y1 = y.asInstanceOf[Small].x
              SafeInt(x1.toLong * y1.toLong)
            } else {
              it.slowpath(); Big(x.toBigInt.multiply(y.toBigInt))
            }
          }
          i += 1
        }
        prod        
      }
      m.asInstanceOf[Small].x
    }
    
    val f2 = it.compile { (n:Int) => 
      var i = 1
      var prod = Small(1)
      while (i < n) {
        prod = Small(prod.x * Small(i).x)
        i += 1
      }
      prod.x    
    }

    val f3 = it.compile { (n:Int) => 
      var i = 1
      var prod = 1
      while (i < n) {
        prod = prod * i
        i += 1
      }
      prod        
    }



    def time(calc: => Int): Int = {
      val t0 = System.currentTimeMillis
      var x = 0
      for (i <- 0 until 100000)
        x |= calc        
      val t1 = System.currentTimeMillis
      println("elapsed " + (t1-t0))
      x
    }



    println("run1")
    time(f(10))
    time(f2(10))
    time(f3(10))
    time(fac(10))
    time(fac2(10))

    println("run2")
    time(f(10))
    time(f2(10))
    time(f3(10))
    time(fac(10))
    time(fac2(10))

    println("run3")
    time(f(10))
    time(f2(10))
    time(f3(10))
    time(fac(10))
    time(fac2(10))

    println("run4")
    time(f(10))
    time(f2(10))
    time(f3(10))
    time(fac(10))
    time(fac2(10))

  }



  class Compiler extends BytecodeInterpreter_TIR_Opt with DefaultMacros {

    def slowpath2() {}
    def dwhile(x: => Boolean)(y: => Unit): Unit = {}

    import com.oracle.graal.api.meta._      // ResolvedJavaMethod


    override def handleMethodCall(parent: InterpreterFrame, m: ResolvedJavaMethod): Option[InterpreterFrame] = {
      val className = m.getDeclaringClass.toJava.getName
      val fullName = className + "." + m.getName
      var continuation: InterpreterFrame = parent
      def handle(f: List[Rep[Object]] => Rep[Object]): Option[InterpreterFrame] = {
        val returnValue = f(popArgumentsAsObject(parent, m, !java.lang.reflect.Modifier.isStatic(m.getModifiers)).toList)
        
        // NOTE: we should take m.getSignature below (see delite test)
        // not quite ... if we're not returning to out continuation we have a different type!
        //println("return "+returnValue+"/"+returnValue.typ+"/"+m.getSignature().getReturnKind)
        //println("to "+continuation.getMethod+"/"+continuation.getMethod.getSignature().getReturnKind)

        //pushAsObject(continuation, m.getSignature().getReturnKind(), returnValue)
        if (continuation == parent)
          pushAsObject(continuation, m.getSignature().getReturnKind(), returnValue)
        else
          pushAsObject(continuation, continuation.getMethod.getSignature().getReturnKind(), returnValue)
        // better guess? take return kind from returnValue.typ??

        Some(if (continuation == parent) null else continuation)
      }

      if (traceMethods) Console.println("// "+fullName)

      val exits = "reflMethod"::Nil

      if (exits.exists(fullName contains _)) return handle { case _ => reflect[Unit]("()/*"+fullName+"*/").asInstanceOf[Rep[Object]] }

      // check for known methods
      fullName match {
        case "java.lang.StringBuilder.append" => handle {
          case _ => reflect[Unit]("()/*java.lang.StringBuilder.append*/").asInstanceOf[Rep[Object]]
        }
        case _ if fullName.contains("dwhile") => handle {
          case o::(c:Rep[() => Boolean])::(b:Rep[() => Unit])::Nil => 
            val ci = decompileInternal0[Boolean](c)
            val bi = decompileInternal0[Unit](b)
            reflect[Unit]("while ({",ci,"}) {",bi,"}").asInstanceOf[Rep[Object]]
        }
        case _ => 
          //println(fullName)
          super.handleMethodCall(parent,m)
      }
    }


    initialize()
    traceMethods = false
    emitUniqueOpt = true
    emitRecursive = false
    emitCheckCast = false
    emitArrayChecks = false
    emitNullChecks = false
    debugBlockKeys = false
    debugReadWrite = false
    debugMethods = false
    debugReturns = false

  }

}
