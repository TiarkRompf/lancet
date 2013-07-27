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
package analysis

class TestAnalysis7 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-7"

/* 
  compilers from definitional interpreters. defunctionalization, cps conversion.
*/

  object Test1 {

    trait PCF {
      type Rep[T]

      def nat(c: Int): Rep[Int]
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int]
      def times(x: Rep[Int], y: Rep[Int]): Rep[Int]
      def ifz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T]

      def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B]
      def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B]
      def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B]

      // let fix f = ( let rec self n = f self n in self )

    }

    trait PCF_Direct extends PCF {
      type Rep[T] = T

      def nat(c: Int): Rep[Int] = c
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int] = x + y
      def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = x * y
      def ifz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T] = if (c == 0) a else b

      def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B] = f
      def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = f(x)
      def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = { def self(x:A):B = f(self)(x); self }

      // let fix f = ( let rec self n = f self n in self )
    }

    trait PCF_String extends PCF {
      type Rep[T] = String

      var nVars = 0
      def fresh[A] = { nVars += 1; "x"+(nVars-1) }


      def nat(c: Int): Rep[Int] = s"$c"
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int] = s"$x + $y"
      def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = s"$x * $y"
      def ifz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T] = s"if ($c == 0) { $a } else { $b }"

      def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B] = { 
        val x = fresh[A]; s"{ $x => ${f(x)} }" 
      }
      def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = s"$f($x)"
      def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = { 
        val self = fresh[A=>B]
        s"def $self:A=>B = ${f(self)}; $self"
      }

      // let fix f = ( let rec self n = f self n in self )
    }


    // defunctionalization and cps conversion

    trait PCF_Env extends PCF {
      type Rep[T] = String

      var nVars = 0
      def fresh[A] = { nVars += 1; "x"+(nVars-1) }

      var nLabels = 0
      def label[A] = { nLabels += 1; "f"+(nLabels-1) }

      def reflect[A](s: String) = { val x = fresh[A]; println(s"  val $x = $s"); x }

      def nat(c: Int): Rep[Int] = s"$c"
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(s"$x + $y")
      def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(s"$x * $y")


      type Env = (Int, String)
      def getEnv = (nVars, (0 until nVars).map("x" + _).mkString(","))
      def setEnv(p: Env) = nVars = p._1

      type Clo = (String,Env)
      def applyK(p: Clo,x: String): String = s"${p._1}(${p._2._2})($x)"


      def gotoBlock(l: String, e: Env): String = s"$l(${e._2})"
      def gotoBlock(l: String, e: Env, x:String): String = s"$l(${e._2})($x)"
      def startBlock(l: String, e: Env) = println(s"def $l(${e._2}) = {")
      def startBlock(l: String, e: Env, x: String) = println(s"def $l(${e._2})($x) = {")
      def startBlock(l: String, e: Env, x: String, y: String) = println(s"def $l(${e._2})($x)($y) = {")
      def closeBlock() = { println(s"}") }


      def shift(k: Clo => String): String = {
        val fc = label[Any]
        val env = getEnv
        val res = k((fc,env))
        println(res)
        closeBlock()
        //setEnv(env)
        val r = fresh[Any]
        startBlock(fc,env, r)
        r
      }

      def block(f: => String) = {
        val fa = label[Any]
        val env = getEnv
        startBlock(fa,env)
          val ra = f
          setEnv(env)
          println(s"  " + ra)
        closeBlock()

        gotoBlock(fa,env)
      }

      def block2(f: (String,String) => String): Clo = {
        val fa = label[Any]
        val env = getEnv
        val x = fresh[Any]
        val k = fresh[Any]
        startBlock(fa,env,x,k)
          val ra = f(x,k)
          setEnv(env)
          println(s"  " + ra)
        closeBlock()

        (fa,env)
      }

      def clos(f: Clo) = f match { case (fk,(_,args)) => s"Clos($fk)($args)" }
      def closApp(f: String, x: String) = s"  $f.apply($x)"
      def closApp(f: String, x: String, k: String) = s"  $f.apply($x)($k)"

      def ifInternal[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T] = {
        s"  if ($c == 0) ${ a } else ${ b }"
      }


      def ifz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T] = {
        shift { fr => 
          ifInternal(c, block { applyK(fr, a) }, block { applyK(fr, b) })
        }
      }

      def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B] = {
        println("// build lambda")
        val fx = block2 { (x,k) =>
          closApp(k,f(x))
        }

        reflect(clos(fx))
      }

      def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = {
        shift { fr => 
          closApp(f,x,clos(fr))
        }
      }


      def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = {
        f("self")
      }

      // let fix f = ( let rec self n = f self n in self )
    }



    def run[T](f: => T): Unit = {
      println(s"def main = {")
      val r = f
      println(s"  $r")
      println(s"}")
    }

  }


  // mini interpreter for deep embedding

  object Test2 {

    type Ident = String
    abstract class Exp

    case class Lit(x: Val) extends Exp
    case class Op1(o: OP, x: Exp) extends Exp
    case class Op2(o: OP, x: Exp, y: Exp) extends Exp
    case class Var(x: Ident) extends Exp
    case class Lam(x: Ident, y: Exp) extends Exp
    case class Fix(x: Exp) extends Exp
    case class App(x: Exp, y: Exp) extends Exp
    case class Ifz(x: Exp, y: Exp, z: Exp) extends Exp

    type Val = Any
    type Env = Ident => Val
    type OP = String

    def eval(x: Exp, e: Env): Val = x match {
      case Lit(x)       => x
      case Op1("!",x)   => !eval(x,e).asInstanceOf[Boolean]
      case Op1("-",x)   => -eval(x,e).asInstanceOf[Int]
      case Op2("+",x,y) => eval(x,e).asInstanceOf[Int] + eval(x,e).asInstanceOf[Int]
      case Op2("*",x,y) => eval(x,e).asInstanceOf[Int] * eval(x,e).asInstanceOf[Int]
      case Var(x)       => e(x)
      case Lam(x,y)     => (z: Val) => eval(y, (k:Ident) => if (k == x) z else e(k))
      case App(x,y)     => eval(x,e).asInstanceOf[Val=>Val](eval(y,e))
    }
  }


  // abstract env and primitives
  // defunctionalize closures

  object Test3 {

    type Ident = String
    abstract class Exp

    case class Lit(x: Val) extends Exp
    case class Op1(o: OP, x: Exp) extends Exp
    case class Op2(o: OP, x: Exp, y: Exp) extends Exp
    case class Var(x: Ident) extends Exp
    case class Lam(x: Ident, y: Exp) extends Exp
    case class Fix(x: Exp) extends Exp
    case class App(x: Exp, y: Exp) extends Exp
    case class Ifz(x: Exp, y: Exp, z: Exp) extends Exp

    type Env = Ident => Val

    def envInit: Env = (x: Ident) => ???
    def envGet(e: Env, x: Ident) = e(x)
    def envPlus(e: Env, x: Ident, y: Val) = (k: Ident) => if (k == x) y else e(k)

    var env = envInit
    def stateEnv: Env = env
    def stateEnvWith[A](e: Env)(block: =>A): A = { var save = env; env = e; try block finally env = save }

    type Val = Any
    type OP = String

    def primOp1(o: OP, x: Val): Val = (o,x) match {
      case ("!", x: Boolean) => !x
      case ("-", x: Int) => -x
    }
    def primOp2(o: OP, x: Val, y: Val): Val = (o,x,x) match {
      case ("+", x: Int, y: Int) => x + y
      case ("*", x: Int, y: Int) => x * y
    }

    def funVal1(x: Ident, y: Exp, e: Env): Val = { z: Val => stateEnvWith(envPlus(e,x,z)) { eval(y) } }
    def funApply1(x: Val, y: Val): Val = x match { case f: (Val=>Val) => f(y) }

    case class Clos(x: Ident, y: Exp, e: Env)
    def funVal(x: Ident, y: Exp, e: Env): Val = Clos(x,y,e)
    def funApply(x: Val, z: Val): Val = x match { case Clos(x,y,e) => stateEnvWith(envPlus(e,x,z)) { eval(y) } }

    def eval(x: Exp): Val = x match {
      case Lit(x)     => x
      case Op1(o,x)   => primOp1(o,eval(x))
      case Op2(o,x,y) => primOp2(o,eval(x),eval(y))
      case Var(x)     => envGet(stateEnv,x)
      case Lam(x,y)   => funVal(x,y,stateEnv)
      case App(x,y)   => funApply(eval(x),eval(y))
    }
  }

  // cps convert / explicit stack (unfinished)

  object Test4 {

    type Ident = String
    abstract class Exp

    case class Lit(x: Val) extends Exp
    case class Op1(o: OP, x: Exp) extends Exp
    case class Op2(o: OP, x: Exp, y: Exp) extends Exp
    case class Var(x: Ident) extends Exp
    case class Lam(x: Ident, y: Exp) extends Exp
    case class Fix(x: Exp) extends Exp
    case class App(x: Exp, y: Exp) extends Exp
    case class Ifz(x: Exp, y: Exp, z: Exp) extends Exp

    type Env = Ident => Val

    def envInit: Env = (x: Ident) => ???
    def envGet(e: Env, x: Ident) = e(x)
    def envPlus(e: Env, x: Ident, y: Val) = (k: Ident) => if (k == x) y else e(k)

    var env = envInit
    def stateEnv: Env = env
    def stateEnvWith[A](e: Env)(block: =>A): A = { var save = env; env = e; try block finally env = save }

    type Val = Any
    type OP = String

    def primOp1(o: OP, x: Val): Val = (o,x) match {
      case ("!", x: Boolean) => !x
      case ("-", x: Int) => -x
    }
    def primOp2(o: OP, x: Val, y: Val): Val = (o,x,x) match {
      case ("+", x: Int, y: Int) => x + y
      case ("*", x: Int, y: Int) => x * y
    }

    def funVal1(x: Ident, y: Exp, e: Env): Val = { z: Val => stateEnvWith(envPlus(e,x,z)) { eval(y) } }
    def funApply1(x: Val, y: Val): Val = x match { case f: (Val=>Val) => f(y) }

    case class Clos(x: Ident, y: Exp, e: Env)
    def funVal(x: Ident, y: Exp, e: Env): Val = Clos(x,y,e)
    def funApply(x: Val, z: Val): Val = x match { case Clos(x,y,e) => stateEnvWith(envPlus(e,x,z)) { eval(y) } }

    type Ctx = Val => Unit

    var state: List[(Exp,Ctx)] = Nil
    var k: Ctx = { x => println("TOP: "+x) }

    def step(x: Exp) = x match {
      case Lit(x)     => k(x)
      case Op1(o,x)   => 
        val k0 = k
        k = { z => k0(primOp1(o,z)) }
        eval(x)
      case Op2(o,x,y) => 
        val k0 = k
        k = { z1 => 
          k = { z2 => 
            k0(primOp2(o,z1,z2))
          }
          eval(y)
        }
        eval(x)
      case Var(x)     => k(envGet(env,x))
      case Lam(x,y)   => k(Clos(x,y,env))
      case App(x,z)   => 
        val k0 = k
        val e0 = env
        k = { case Clos(x,y,e) => 
          k = { z => 
            k = { res =>
              env = e0
              k0(res)
            }
            eval(y)
          }
          eval(z)
        }
        eval(x)
    }

    def eval(x: Exp): Val = x match {
      case Lit(x)     => x
      case Op1(o,x)   => primOp1(o,eval(x))
      case Op2(o,x,y) => primOp2(o,eval(x),eval(y))
      case Var(x)     => envGet(stateEnv,x)
      case Lam(x,y)   => funVal(x,y,stateEnv)
      case App(x,y)   => funApply(eval(x),eval(y))
    }
  }



  // *** test 

  def testA = withOutFileChecked(prefix+"A") {
    import Test1._

    val eval: PCF = new PCF_Direct {}
    val comp: PCF = new PCF_String {}

    def df(IR: PCF) = {
      import IR._
      val fac = fix[Int,Int] { fac => lam[Int,Int] { n => 
        ifz(n, nat(1), times(n, app(fac, plus(n,nat(-1)))))
      }}
      app(fac,nat(4))
    }

    printcheck(df(eval),24)
    println("----")
    println(df(new PCF_String {}))
    println("----")
    println(Test1.run(df(new PCF_Env {})))
  }


}