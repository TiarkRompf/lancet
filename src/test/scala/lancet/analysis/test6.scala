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

class TestAnalysis6 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-6"

/* 
  logic programming with constraints
*/

  object Test1 {

    // *** run loop

    def run[T](f: Exp[T] => Rel): Unit = {
      cstore = cstore0
      varCount = varCount0

      var d = 0
      def printd(x: Any) = println(" "*d+x)

      def rec(e: () => Rel)(f: () => Unit): Unit = { 
        //printd("rec: "+e)
        if (d == 2000) {
          printd("ABORT depth "+d)
          return
        }
        val d1 = d
        val save = cstore
        d += 1
        val r = e() match {
          case Or(a,b) =>
            rec(a)(f)
            rec(b)(f)
          case And(a,b) =>
            rec(a) { () =>
              if (propagate())
                rec(b)(f)
            }
          case Yes => f()
        }
        cstore = save
        d = d1
        r
      }

      def propagate(): Boolean = { // propagate constraints and look for contradictions
        //printd("simplify")
        val cnew = cstore flatMap { c1 => cstore flatMap { c2 => (c1,c2) match {
          case (IsEqual(Exp(a),Exp(b)), IsTerm(a1, key, args)) if a == a1 => 
            List(IsTerm(b, key, args))
          case (IsEqual(Exp(a),Exp(b)), IsTerm(b1, key, args)) if b == b1 => 
            List(IsTerm(a, key, args))
          case (IsTerm(a1, key1, args1), IsTerm(a2, key2, args2)) if a1 == a2 =>
            if (key1 != key2 || args1.length != args2.length) return false
            (args1,args2).zipped map (IsEqual(_,_))
          case _ => Nil
        }}}

        //cnew filterNot (cstore contains _) foreach println

        val cstore0 = cstore
        cstore = (cstore ++ cnew).distinct.sortBy(_.toString)
        (cstore == cstore0) || propagate() // until converged
      }

      def extract(x: Exp[Any]): String = cstore collectFirst { // extract term
        case IsTerm(id, key, args) if id == x.id => 
          key+"("+args.map(extract).mkString(",")+")"
      } getOrElse canon(x)

      def canon(x: Exp[Any]): String = { // canonicalize var name
        val id = (x.id::(cstore collect {
          case IsEqual(`x`,y) if y.id < x.id => y.id
          case IsEqual(y,`x`) if y.id < x.id => y.id
        })).min
        "x"+id
      }

      val q = fresh[T]
      rec(() => f(q)){() => 
        if (propagate()) {
          //printd("success!")
          //printd(eval(q))
          //cstore foreach { c => printd("    "+c)}
          println(extract(q))
        }
      }
      println("----")
    }


    // *** terms and constraints

    case class Exp[+T](id: Int)

    val varCount0 = 0
    var varCount = varCount0
    def fresh[T] = Exp[T] { varCount += 1; varCount - 1 }

    abstract class Constraint
    case class IsTerm(id: Int, key: String, args: List[Exp[Any]]) extends Constraint
    case class IsEqual(x: Exp[Any], y: Exp[Any]) extends Constraint

    abstract class Rel
    case class Or(x: () => Rel, y: () => Rel) extends Rel
    case class And(x: () => Rel, y: () => Rel) extends Rel
    case object Yes extends Rel



    val cstore0: List[Constraint] = Nil
    var cstore: List[Constraint] = cstore0

    def register(c: Constraint): Unit = {
      cstore = c::cstore // start simplify right here?
    }

    def term[T](key: String, args: List[Exp[Any]]): Exp[T] = {
      val id = fresh[T]
      val c = IsTerm(id.id, key, args)
      register(c)
      id
    }

    def exists[T](f: Exp[T] => Rel): Rel = {
      f(fresh[T])
    }

    def exists[T,U](f: (Exp[T],Exp[U]) => Rel): Rel = {
      f(fresh[T],fresh[U])
    }

    def exists[T,U,V](f: (Exp[T],Exp[U],Exp[V]) => Rel): Rel = {
      f(fresh[T],fresh[U],fresh[V])
    }

    def infix_===[T](a: => Exp[T], b: => Exp[T]): Rel = {
      val c = IsEqual(a,b)
      register(c)
      Yes
    }
    def infix_&&(a: => Rel, b: => Rel): Rel = {
      And(() => a,() => b)
    }
    def infix_||(a: => Rel, b: => Rel): Rel = {
      Or(() => a,() => b)
    }
  }


  // *** test 

  def testA = withOutFileChecked(prefix+"A") {
    import Test1._

    def list(xs: String*): Exp[List[String]] = if (xs.isEmpty) nil else cons(term(xs.head,Nil),list(xs.tail:_*))

    def cons[T](hd: Exp[T], tl: Exp[List[T]]): Exp[List[T]] = term("cons",List(hd,tl))
    def nil: Exp[List[Nothing]] = term("nil",List())
    def pair[A,B](a: Exp[A], b: Exp[B]): Exp[(A,B)] = term("pair",List(a,b))

    object Cons {
      def unapply[T](x: Exp[List[T]]): Some[(Exp[T],Exp[List[T]])] = {
        val h = fresh[T]
        val t = fresh[List[T]]
        x === cons(h,t)
        Some((h,t))
      }
    }
    object Pair {
      def unapply[A,B](x: Exp[(A,B)]): Some[(Exp[A],Exp[B])] = {
        val a = fresh[A]
        val b = fresh[B]
        x === pair(a,b)
        Some((a,b))
      }
    }

    def append[T](as: Exp[List[T]], bs: Exp[List[T]], cs: Exp[List[T]]): Rel = 
      (as === nil && bs === cs) || 
      exists[T,List[T],List[T]] { (h,t1,t2) => 
        (as === cons(h,t1)) && (cs === cons(h,t2)) && append(t1,bs,t2)
      }


    Test1.run[List[String]] { q =>
      append(list("a","b","c"), list("d","e","f"), q)
    }

    Test1.run[List[String]] { q =>
      append(list("a","b","c"), q, list("a","b","c","d","e","f"))
    }

    Test1.run[List[String]] { q =>
      append(q, list("d","e","f"), list("a","b","c","d","e","f"))
    }

    Test1.run[(List[String],List[String])] { q =>
      val q1,q2 = fresh[List[String]]
      (q === pair(q1,q2)) &&
      append(q1, q2, list("a","b","c","d","e","f"))
    }

    Test1.run[(List[String],List[String])] { 
      case Pair(q1,q2) =>
        append(q1, q2, list("a","b","c","d","e","f"))
    }

    Test1.run[(List[String],List[String])] { 
      case Pair(q1,q2) => q1 === q2
    }


  }




}