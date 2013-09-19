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

import scala.collection.mutable

class TestAnalysis9 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-9"


/*
  Interpreter, compiler, and analyzer

    - introduce language syntax and
      definitional interpreter
    - interpreter explicitly checks
      whether variables have been defined

    - we would like to turn this interpreter
      into a compiler
    - we do so mechanically, by quoting all
      expressions

    - the generated code is not very good,
      all checks still need to be performed

    - we would like to implement a static
      analysis that determines whether a
      program is safe. 
    - if we only compile checked programs,
      we can remove the runtime checks

  Abstracting the interpreter

    - we had to implement the three artifacts
      essentially from scratch. in hindsight,
      there are lots of commonalities

    - starting from the concrete interpreter,
      we build an abstract interpreter.
    - 'abstract' is to be understood foremost
      as in representation independence
    - we will recover 'abstract' as in
      Cousot's abstract interpretation shortly

    - abstracting values: instead of Int, use
      an abstract type. interpretations need
      to provide the int operations (+,*).
    - abstracting the store: for the moment
      we can keep a static map of addresses
      to abstract values (since addresses
      are not first class). we will see more
      involved store models later.
    - abstract control flow and state:
      this one is important. we need to execute
      both conditional branches for analysis.

  Abstract interpretation

    TODO:
    - partial evaluation as abstract interpretation
    - supercompilation

    - we first show more or less classical
      abstract interpretation. both if/else and
      while loops use a single lub operator.
    - the result is not very precise. we do
      not have strong updates.


  Precise analysis

    - want to use a relational domain
    - key new idea: use programs themselves as
      abstract domain (relation to supercomp?)
    - optimize abstract program using
      rewriting: not for execution, but for
      analysis

    - like in gated SSA, if/else join operator
      carries condition
    - apply the same idea to loops: index
      abstract values by loop trip count

    - guiding principle: transform recursive
      functions (loop carried deps) to closed
      form

    - integer recurrences -> polynomials

    - introduce objects. addresses become
      first class.

    - closed form for array creation

    - introduce dynamic allocations.
      model (infinite) abstract address space
      as arrays indexed by loop count.


  Higher order languages

    - TBD
    - (first order) functions, recursion, stack
      - model stack as linked list
    - lambdas: defunctionalize?


*/



  // syntax
  case class Prog(a: List[String], b: Exp)
  abstract class Exp
  case class Lit(c: Int) extends Exp
  case class Plus(a: Exp, b: Exp) extends Exp
  case class Times(a: Exp, b: Exp) extends Exp
  case class Get(a: String) extends Exp
  case class Put(a: String, b: Exp) extends Exp
  case class Block(as: List[Exp]) extends Exp
  case class If(c: Exp, a: Exp, b: Exp) extends Exp
  case class While(c: Exp, b: Exp) extends Exp

  // issues: 
  //  - Block(Nil) failure
  //  - add assert stm ?

  //fac(n) = n * fac(n-1)
  val fac = Prog(List("n"),Block(List(
    Put("r",Lit(1)),
    While(Get("n"),Block(List(
      Put("r",Times(Get("r"),Get("n"))),
      Put("n",Plus(Get("n"),Lit(-1)))
    ))),
    Get("r")
  )))


  def test1 = withOutFileChecked(prefix+"A") {


    // definitional interpreter
    val store = mutable.Map[String,Int]()
    def eval(e: Exp): Int = e match {
      case Lit(c)      => c
      case Plus(a,b)   => eval(a) + eval(b)
      case Times(a,b)  => eval(a) * eval(b)
      case Get(a)      => if (!store.contains(a)) error(s"not found: $a")
                          store(a)
      case Put(a,b)    => store(a) = eval(b); store(a)
      case Block(as)   => as.map(eval).last
      case If(c,a,b)   => if (eval(c) != 0) eval(a) else eval(b)
      case While(c,b)  => while (eval(c) != 0) eval(b); 0
    }
    def evalp(p: Prog, args: List[Int]): Int = p match {
      case Prog(in,e)  =>
        if (args.length != in.length) 
          error("wrong number or arguments")
        store.clear
        store ++= in zip args
        eval(e)
    }


    println("eval:")
    println(evalp(fac,List(4)))


    // static checker
    var defined = Set[String]()
    def check(e: Exp): Unit = e match {
      case Lit(c)      => 
      case Plus(a,b)   => check(a); check(b)
      case Times(a,b)  => check(a); check(b)
      case Get(a)      => if (!defined.contains(a)) error(s"not found: $a")
      case Put(a,b)    => check(b); defined += a
      case Block(as)   => as.foreach(check)
      case If(c,a,b)   => check(c); 
                          val common = defined
                          check(a)
                          val onlyA = defined
                          defined = common
                          check(b)
                          val onlyB = defined
                          defined = onlyA intersect onlyB
      case While(c,b)  => check(c)
                          val onlyC = defined
                          check(b)
                          defined = onlyC
    }
    def checkp(p: Prog) = p match {
      case Prog(in,e) =>
        defined = in.toSet
        check(e)
    }

    println("---")
    println("check:")
    checkp(fac)


    // compiler 1
    {
    def compile(e: Exp): List[String] = e match {
      case Lit(c)      => s"$c"::Nil
      case Plus(a,b)   => s"(${compileb(a)} + ${compileb(b)})"::Nil
      case Times(a,b)  => s"(${compileb(a)} * ${compileb(b)})"::Nil
      case Get(a)      => s"if (!store.contains($a)) error(not found: $a)"::s"store($a)"::Nil
      case Put(a,b)    => s"$a = ${compileb(b)}"::s"$a"::Nil
      case Block(as)   => as.flatMap(compile)
      case If(c,a,b)   => s"if (${compileb(c)} != 0) ${compileb(a)} else ${compileb(b)}"::Nil
      case While(c,b)  => s"while (${compileb(c)} != 0) ${compileb(b)}"::"0"::Nil
    }
    def compileb(e: Exp): String = block(compile(e)) 
    def block(ss: List[String]): String = ss match {
      case List(s) => s
      case ss => "{\n  " + ss.mkString("\n").replaceAll("\\n","\n  ") + "\n}"
    }
    def compilep(p: Prog) = p match {
      case Prog(in,e) =>
        defined = in.toSet
        val c = compile(e)
        "def prog(" + in.map(a=>a+"0: Int").mkString(",") + ") = " + 
        block("val store = new HashMap[String,Int] "::in.map(a=>s"store($a) = ${a}0").mkString(";")::c)
    }

    println("---")
    println("compile1:")
    println(compilep(fac))
    }

    {
    // compiler 2
    def compile(e: Exp): List[String] = e match {
      case Lit(c)      => s"$c"::Nil
      case Plus(a,b)   => s"(${compileb(a)} + ${compileb(b)})"::Nil
      case Times(a,b)  => s"(${compileb(a)} * ${compileb(b)})"::Nil
      case Get(a)      => s"$a"::Nil
      case Put(a,b)    => defined += a
                          s"$a = ${compileb(b)}"::s"$a"::Nil
      case Block(as)   => as.flatMap(compile)
      case If(c,a,b)   => s"if (${compileb(c)} != 0) ${compileb(a)} else ${compileb(b)}"::Nil
      case While(c,b)  => s"while (${compileb(c)} != 0) ${compileb(b)}"::"0"::Nil
    }
    def compileb(e: Exp): String = block(compile(e)) 
    def block(ss: List[String]): String = ss match {
      case List(s) => s
      case ss => "{\n  " + ss.mkString("\n").replaceAll("\\n","\n  ") + "\n}"
    }
    def compilep(p: Prog) = p match {
      case Prog(in,e) =>
        defined = in.toSet
        val c = compile(e)
        "def prog(" + in.map(a=>a+"0: Int").mkString(",") + ") = " + 
        block("var " + defined.mkString(",") + ": Int = 0"::in.map(a=>a+" = "+a+"0").mkString(";")::c)
    }

    println("---")
    println("compile2:")
    println(compilep(fac))
    }

  }

  def test2 = withOutFileChecked(prefix+"B") {

    // definitional abstract interpreter

    trait Base {

      type Val
      def infix_+(a: Val, b: Val): Val
      def infix_*(a: Val, b: Val): Val
      def infix_!=(a: Val, b: Val): Boolean

      implicit def lift(x: Int): Val

    }

    val Exec: Base = new Base {
      type Val = Int
      def infix_+(a: Val, b: Val): Val = a + b
      def infix_*(a: Val, b: Val): Val = a * b
      def infix_!=(a: Val, b: Val): Boolean = a != b
      def lift(x: Int) = x
    }
    import Exec._

    val store = mutable.Map[String,Val]()
    def eval(e: Exp): Val = e match {
      case Lit(c)      => c
      case Plus(a,b)   => eval(a) + eval(b)
      case Times(a,b)  => eval(a) * eval(b)
      case Get(a)      => if (!store.contains(a)) error(s"not found: $a")
                          store(a)
      case Put(a,b)    => store(a) = eval(b); store(a)
      case Block(as)   => as.map(eval).last
      case If(c,a,b)   => if (eval(c) != 0) eval(a) else eval(b)
      case While(c,b)  => while (eval(c) != 0) eval(b); 0
    }
    def evalp(p: Prog, args: List[Val]): Val = p match {
      case Prog(in,e)  =>
        if (args.length != in.length) 
          error("wrong number or arguments")
        store.clear
        store ++= in zip args
        eval(e)
    }

    println("eval:")
    println(evalp(fac,List(4)))

  }

  def test3 = withOutFileChecked(prefix+"C") {

    // definitional abstract interpreter

    trait Base extends EmbeddedControls {

      type Val
      def infix_+(a: Val, b: Val): Val
      def infix_*(a: Val, b: Val): Val
      def infix_nonZero(a: Val): Cond

      type Cond

      def if_ (c: Cond)(a: => Val)(b: => Val): Val
      def while_(c: => Cond)(b: => Val): Unit

      implicit def lift(x: Int): Val

      //def __ifThenElse[T](c: Boolean, a: => T, b: => T): T = c match { case true => a case false => b } // bug
    }

    val Exec: Base = new Base {
      type Val = Int
      def infix_+(a: Val, b: Val): Val = a + b
      def infix_*(a: Val, b: Val): Val = a * b
      def infix_nonZero(a: Val): Cond = a != 0
      def lift(x: Int) = x
      type Cond = Boolean
      def if_(c: Cond)(a: => Val)(b: => Val): Val = if (c) a else b
      def while_(c: => Cond)(b: => Val): Unit = while(c) b
    }
    import Exec._

    val store = mutable.Map[String,Val]()
    def eval(e: Exp): Val = e match {
      case Lit(c)      => c
      case Plus(a,b)   => eval(a) + eval(b)
      case Times(a,b)  => eval(a) * eval(b)
      case Get(a)      => if (!store.contains(a)) error(s"not found: $a")
                          store(a)
      case Put(a,b)    => store(a) = eval(b); store(a)
      case Block(as)   => as.map(eval).last
      case If(c,a,b)   => 
        
        val cond = eval(c).nonZero

        //val st0 = getState
        //val st1 = if (cond.maybeTrue) { 
        
        if_ (eval(c).nonZero) { eval(a) } { eval(b) }

      case While(c,b)  => 

        while_ (eval(c).nonZero) { eval(b) }; 0
    }
    def evalp(p: Prog, args: List[Val]): Val = p match {
      case Prog(in,e)  =>
        if (args.length != in.length) 
          error("wrong number or arguments")
        store.clear
        store ++= in zip args
        eval(e)
    }

    println("eval:")
    println(evalp(fac,List(4)))

  }  



}
