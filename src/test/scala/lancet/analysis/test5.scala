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

class TestAnalysis5 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-5"

/* 
  DOT-style store abstraction with paths (i.e. path dependent types)

  With: 
    - instrumented semantics (preserve static typings)
    - singleton types, dependent method types

*/

  object Test1 {

    abstract class Exp {
      override def toString = prettyExp(this)
    }
    case class Var(s: String) extends Exp
    case class Sel(x: Exp, l: String) extends Exp
    case class Subst(x: Exp, a: List[String], b: Exp) extends Exp
    case class Let(x: String, y: Exp, z: Exp) extends Exp
    case class App(x: Exp, l: String, y: Exp) extends Exp
    case class New(tp: Typ, ls: Map[String,Exp], ms: Map[String,(String,Exp)]) extends Exp

    abstract class Typ {
      override def toString = prettyTyp(this)
    }
    case class TAnd(x: Typ, y: Typ) extends Typ
    case class TOr(x: Typ, y: Typ) extends Typ
    case class TSel(x: Exp, l: String) extends Typ
    case class TStruct(y: String, ts: Map[String,(Typ,Typ)], ls: Map[String,Typ], ms: Map[String,(String,Typ,Typ)]) extends Typ
    case object TTop extends Typ
    case object TBot extends Typ

    case class TRef(x: Exp) extends Typ
    case class TObj(n: String, x: Typ) extends Typ // alternative: put x in a separate type store

    def prettyExp(e: Exp) = e match {
      case Var(s)        => s
      case Sel(x,l)      => s"$x.$l"
      case Subst(x,a,b)  => s"($x)[${a.mkString(",")}->$b]"
      case Let(x,y,z)    => s"val $x = $y\n$z"
      case App(x,l,y)    => s"$x.$l($y)"
      case New(tp,ls,ms) => if ((ls++ms).nonEmpty) s"new $tp {\n  ${ (ls++ms).mkString("\n").replace("\n","\n  ") }\n}" else s"new $tp { }"
    }

    def prettyTyp(t: Typ) = t match {
      case TAnd(x,y)     => s"$x ∧ $y"
      case TOr(x,y)      => s"$x ∨ $y"
      case TSel(x,l)     => s"$x.$l"
      case TTop          => s"⊤"
      case TBot          => s"⊥"
      case TRef(x)       => s"$x"
      case TObj(n,x)     => s"#$n = $x"
      case TStruct(y,ts,ls,ms) => if ((ts++ls++ms).nonEmpty) (s"{ $y => \n  ${ (ts++ls++ms).mkString("\n").replace("\n","\n  ") }\n}") else s"{ $y => }"
    }      


    // *** evaluation

    type Loc = Int
    type Env = Map[String,Loc]

    case class Obj(env: Env, tp: Typ, ls: Map[String,Loc], ms: Map[String,(String,Exp)])

    var store: Map[Loc,Obj] = Map.empty

    def eval(e: Exp, env: Env): Loc = e match {
      case Var(s)        => env(s)
      case Sel(x,l)      => store(eval(x,env)).ls(l)
      //case Let(x,y,z)    => eval(z,env+(x->eval(y,env)))
      case App(x,l,y)    => 
        val (v,z) = store(eval(x,env)).ms(l)
        eval(z,env+(v->eval(y,env)))
      case Let(x,New(tp,ls,ms),z) => 
        val loc = store.size
        // init fields. object name is accessible, fields are undefined
        store = store + (loc -> Obj(env, tp, Map.empty, ms))
        val ls1 = ls.map(p=>p._1 -> eval(p._2, env + (x->loc)))
        // now store complete object
        store = store + (loc -> Obj(env + (x->loc), tp, ls1, ms))
        eval(z, env + (x->loc))
    }

    // *** instrumented evaluation (for checking soundness, pass along types)

    case class Val(loc: Int, typ: Typ) { def widen(t: Typ) = Val(loc,t) } // could check this ...
    case class Meth(body: (String,Exp), typ: (String,Typ,Typ))
    type XEnv = Map[String,Val]

    def xstore(x: Val) = new {
      def ls(l: String, p: Exp, e: XEnv) = Val(store(x.loc).ls(l), texpand(x.typ, p, e.mapValues(_.typ)).ls(l))
      def ms(l: String, p: Exp, e: XEnv) = Meth(store(x.loc).ms(l), texpand(x.typ, p, e.mapValues(_.typ)).ms(l))
      def lsOrig(l: String, p: Exp, e: XEnv) = Val(store(x.loc).ls(l), texpand(store(x.loc).tp, p, e.mapValues(_.typ)).ls(l))
      def msOrig(l: String, p: Exp, e: XEnv) = Meth(store(x.loc).ms(l), texpand(store(x.loc).tp, p, e.mapValues(_.typ)).ms(l))
    }

    def xeval(e: Exp, env: XEnv): Val = e match {
      case Var(s)        => env(s)
      case Sel(x,l)      => xstore(xeval(x,env)).ls(l,x,env)
      case App(x,l,y)    => 
        val receiver = xeval(x,env)
        val Meth(_,    (_,ca,cb)) = xstore(receiver).ms(l,x,env) //type declared at call site
        val Meth((v,z),(p,oa,ob)) = xstore(receiver).msOrig(l,x,env)
        val c = tsubst(ob,List(p),y) // dep method type
        xeval(z,env+(v->xeval(y,env).widen(ca).widen(oa))).widen(ob).widen(cb)
        // NOTE: we need to widen twice, once for the declared type at the call site,
        // and once more for the declared type at object creation (which is the type we
        // should use inside the method body)
      case Let(x,New(tp,ls,ms),z) => 
        val loc = store.size
        val vv = Val(loc,tp)
        // init fields. object name is accessible, fields are undefined
        store = store + (loc -> Obj(env.mapValues(_.loc), tp, Map.empty, ms))
        val ls1 = ls.map(p=>p._1 -> xeval(p._2, env + (x->vv)).loc)
        // now store complete object
        store = store + (loc -> Obj((env + (x->vv)).mapValues(_.loc), tp, ls1, ms))
        xeval(z, env + (x->vv))
    }


    // *** typing

    type TEnv = Map[String,Typ] // env/store

    // eval path
    def tevalp(e: Exp, tenv: TEnv): Typ = {/*println("tevalp " + e)*/; e} match {
      case Var(s)    => tenv(s)
      case Sel(x,l)  => tevald(x,tenv).ls(l)
      case Subst(x,a,b)  => tevalp(tsubstpFull(x,a,b),tenv)
    }

    // eval path and check type conformance
    def tevalc(e: Exp, a: Typ, tenv: TEnv): Typ = tcast(tevalp(e,tenv),a,tenv)

    // eval path and expand
    def tevald(e: Exp, tenv: TEnv): DObj = texpand(tevalp(e,tenv),canonp(e,tenv),tenv)

    // canonicalize path: lookup real obj id (not essential)
    def canonp(e: Exp, tenv: TEnv): Exp = e match {
      case Var(s)    => tenv(s) match {
        // case TObj(n,x) if s != n => canonp(Var(n),tenv)
        // if n doesn't conform to env domain:  Var(tenv.find(_._2 == TObj(n,x)).get._1) // pick first ref in env
        case _ => e
      }
      case Sel(x,l)  => Sel(canonp(x,tenv),l)
    }

    // eval block
    def teval(e: Exp, tenv: TEnv): (Typ,TEnv) = {/*println("TEVAL " + e);tenv.foreach(e=>println("  "+e));tstore.foreach(e=>println("  "+e))*/; e} match {
      case Let(v,App(x,l,y),z) => 
        val (p,a,b) = tevald(x,tenv).ms(l)
        tevalc(y,a,tenv)
        val c = tsubst(b,List(p),y)
        // continue
        teval(z,tenv+(v->c))
      case Let(x,New(tp,ls,ms),z) => 
        val vv = TObj(x,tp)
        val tenv1 = tenv + (x->vv)
        val dec = texpand(tp,Var(x),tenv1)
        // type check decls
        //assert(ls.keys == dec.ls.keys) disabled for the moment ...
        ls.foreach { case (l,e: Var) =>
          tevalc(e, dec.ls(l), tenv1)
        }
        //assert(ms.keys == dec.ms.keys)
        ms.foreach { case (l,(v,r)) =>
          val (p,a,b) = dec.ms(l)
          val c = tsubst(b,List(p),Var(v))
          tevalc(r,c,tenv1 + (v->TObj(v,a))) // FIXME: full eval, not just type eval!
        }
        // continue
        teval(z,tenv1)
      case Let(v,e,z) => 
        val g = tevalp(e,tenv)
        // continue
        teval(z,tenv+(v->g))
      case _ => 
        (tevalp(e,tenv), tenv)
    }


    // *** declaration lattice (check up vs down?)
    case class DObj(ts: Map[String,(Typ,Typ)], ls: Map[String,Typ], ms: Map[String,(String,Typ,Typ)])

    def dand(a: DObj, b: DObj): DObj = {
      val ts = mkmap(a.ts,b.ts) { (s,a,b) => gor(a,b) { case ((a1,a2), (b1,b2)) => (TOr(a1,b1), TAnd(a2,b2)) }}
      val ls = mkmap(a.ls,b.ls) { (s,a,b) => gor(a,b) { TAnd(_,_) }}
      val ms = mkmap(a.ms,b.ms) { (s,a,b) => gor(a,b) { case ((p1,a1,a2), (p2,b1,b2)) => val c2 = tsubst(b2,List(p2),Var(p1)); (p1, TOr(a1,b1), TAnd(a2,c2)) }}
      DObj(ts,ls,ms)
    }
    def dor(a: DObj, b: DObj): DObj = {
      val ts = mkmap(a.ts,b.ts) { (s,a,b) => gand(a, b) { case ((a1,a2), (b1,b2)) => (TAnd(a1,b1), TOr(a2,b2)) }}
      val ls = mkmap(a.ls,b.ls) { (s,a,b) => gand(a, b) { TOr(_,_) }}
      val ms = mkmap(a.ms,b.ms) { (s,a,b) => gand(a, b) { case ((p1,a1,a2), (p2,b1,b2)) => val c2 = tsubst(b2,List(p2),Var(p1)); (p1, TAnd(a1,b1), TOr(a2,c2)) }}
      DObj(ts,ls,ms)
    }

    def mkmap[A,B](a:Map[A,B],b:Map[A,B])(f:(A,Option[B],Option[B])=>Option[B]) = 
      (a.keys++b.keys).toList.flatMap { k => f(k,a.get(k),b.get(k)).toList.map(k -> _) }.toMap

    def gand[A](a: Option[A], b: Option[A])(f: (A,A) => A): Option[A] = (a,b) match {
      case (Some(a), Some(b)) => Some(f(a,b))
      case _ => None
    }
    def gor[A](a: Option[A], b: Option[A])(f: (A,A) => A): Option[A] = (a,b) match {
      case (Some(a), Some(b)) => Some(f(a,b))
      case (Some(a), _) => Some(a)
      case (_, Some(b)) => Some(b)
      case _ => None
    }
    
    // *** expansion
    def texpandp(t: Exp, tenv: TEnv): DObj = 
      if (expanding contains t) { println(s"hang with $t"); DObj(Map(), Map(), Map()) } else {
        val save = expanding; try {
        expanding = t::expanding
        t match {
          case Var(s)       => texpand(tenv(s),t,tenv)
          case Sel(x,l)     => texpand(texpandp(x,tenv).ls(l),t,tenv)
          case Subst(x,a,b) => 
            // WORK IN PROGRESS
            def tsubst1(t:Typ)       = tsubst(t,a,b)
            def tsubst2(t:(Typ,Typ)) = (tsubst1(t._1),tsubst1(t._2))
            def tsubst3(t:(String,Typ,Typ)) = (t._1,tsubst1(t._2),tsubst1(t._3))

            //println(s"expandingp $t $tenv")
            val a0::_ = a
            if (expanding exists { case (`a0`,b1) if b1 != b => { 
              println(s"CONFLICT $a0 -> $b1 / $b"); true } case _ => false }) {
                return DObj(Map(), Map(), Map())
            }
            expanding = ((a0,b))::expanding

            val d = texpandp(tsubstpFull(x,a,b),tenv)
            //println(s"result $d")
            d//DObj(d.ts.mapValues(tsubst2),d.ls.mapValues(tsubst1),d.ms.mapValues(tsubst3))
        }
      } finally { expanding = save }}

    var expanding: List[Any] = Nil
    def texpand(t: Typ, self: Exp, tenv: TEnv): DObj = 
      if (expanding contains t) DObj(Map(), Map(), Map()) else {
        val save = expanding; try {
        expanding = t::expanding
        if (expanding.length > 49) {
          println("abort expanding:")
          expanding.foreach(println)
          assert(false)
        }
        t match { // may not exist for recursive values
          case TTop                 => DObj(Map(), Map(), Map())
          case TBot                 => DObj(Map() withDefaultValue ((TTop,TBot)), Map() withDefaultValue TBot, Map() withDefaultValue (("?",TTop,TBot)))
          case TAnd(x,y)            => dand(texpand(x,self,tenv), texpand(y,self,tenv))
          case TOr(x,y)             => dor(texpand(x,self,tenv), texpand(y,self,tenv))
          case TSel(e,l)            => 
            //println(s"expand sel $t")
            val e1 = texpandp(e,tenv).ts(l)._2
            //println(s"upper bound $e1")
            texpand(e1, self, tenv) // expand upper bound
     
          case TStruct(y,ts,ls,ms)  => 
            def tsubst1(t:Typ)       = tsubst(t,List(y),self)
            def tsubst2(t:(Typ,Typ)) = (tsubst1(t._1),tsubst1(t._2))
            def tsubst3(t:(String,Typ,Typ)) = (t._1,tsubst1(t._2),tsubst1(t._3))
            //dand(texpand(x,self,tenv),DObj(ts.mapValues(tsubst2),ls.mapValues(tsubst1),ms.mapValues(tsubst2)))
            DObj(ts.mapValues(tsubst2),ls.mapValues(tsubst1),ms.mapValues(tsubst3))

          case TRef(x)              => texpandp(x,tenv)//texpand(tenv(x),Var(x),tenv)
          case TObj(n,x)            => texpand(x,self,tenv) // TODO: n
        }
      } finally { expanding = save }}


    // *** subtyping and friends

    // well-formedness -- not implemented (all types expand)
    def twf(t: Typ, tenv: TEnv): Boolean = t match {
      case _ => true //todo
    }
    def twfe(t: Typ, tenv: TEnv): Boolean = t match {
      case _ => true//texpand(t,Var("?"),tenv); twf(t,tenv)
    }
    // identity: must t be the same object as n?
    def tresolve(t: Typ, n: String, tenv: TEnv): Boolean = t match {
      case TObj(`n`,x)           => true
      case TRef(e)               => tresolve(tevalp(e,tenv),n,tenv)
      case TSel(e,l)             => val (s,u) = texpandp(e,tenv).ts(l); tresolve(u,n,tenv);
      case TAnd(a,b)             => tresolve(a,n,tenv) || tresolve(b,n,tenv)
      case TOr(a,b)              => tresolve(a,n,tenv) && tresolve(b,n,tenv)
      case _                     => false
    }
    // subtyping ( do we need path equality for TSel ? )
    def tsub1(t1: Typ, t2: Typ, tenv: TEnv): Boolean = t1 match {
      case TBot                  => twfe(t2,tenv)
      //case TWith(x,_,_,_,_)      => twfe(t1,tenv) && tsub(x,t2,tenv)
      case TStruct(_,_,_,_)      => false//twfe(t1,tenv) && tsub(x,t2,tenv)
      case TSel(e,l)             => val (s,u) = texpandp(e,tenv).ts(l); tsub(s,u,tenv) && tsub(u,t2,tenv)
      case TOr(a,b)              => tsub(a,t2,tenv) && tsub(b,t2,tenv)
      case TAnd(a,b)             => twfe(a,tenv) && tsub(b,t2,tenv) || tsub(a,t2,tenv) && twfe(b,tenv)
      case TRef(e)               => tsub(tevalp(e,tenv),t2,tenv)
      case TObj(n,x)             => tsub(x,t2,tenv)
      case _                     => false
    }
    def tsub2(t1: Typ, t2: Typ, tenv: TEnv): Boolean = t2 match {
      case TTop                  => twfe(t1,tenv)
      //case TWith(x,z,ts,ls,ms)   => twfe(t2,tenv) && tsub(t1,x,tenv) && { val d = texpand(t1,Var(z),tenv); dsub(d, DObj(ts,ls,ms), tenv+(z->t1)) }
      case TStruct(z,ts,ls,ms)   => twfe(t2,tenv) && { val d = texpand(t1,Var(z),tenv); dsub(d, DObj(ts,ls,ms), tenv+(z->t1)) }
      case TSel(e,l)             => val (s,u) = texpandp(e,tenv).ts(l); tsub(s,u,tenv) && tsub(t1,s,tenv)
      case TOr(a,b)              => twfe(a,tenv) && tsub(t1,b,tenv) || tsub(t1,a,tenv) && twfe(b,tenv)
      case TAnd(a,b)             => tsub(t1,a,tenv) && tsub(t1,b,tenv)
      case TRef(e)               => tsub(t1,tevalp(e,tenv),tenv)
      case TObj(n,x)             => tresolve(t1,n,tenv)
      case _                     => false
    }
    def tsub3(t1: Typ, t2: Typ, tenv: TEnv): Boolean = (t1,t2) match {
      // fields are equal if paths point to same obj 
      case (TSel(e1,l1), TSel(e2,l2)) => l1 == l2 && tsub(TRef(e1),TRef(e2),tenv)
      case _ => false
    }
    var depthbudget = 10000
    var subtyping: List[(Typ,Typ)] = Nil
    def tsub(t1: Typ, t2: Typ, tenv: TEnv): Boolean = {
      //println("test subtype: " + t1 + " <:< " + t2)
      if (depthbudget <= 0 || subtyping.contains((t1,t2))) {
        //println("hit depth budget / cycle")
        return false
      }
      val save = subtyping
      try {
        depthbudget -= 1
        subtyping = (t1,t2)::subtyping
        (t1==t2 && twfe(t1,tenv)) || tsub1(t1,t2,tenv) || tsub2(t1,t2,tenv) || tsub3(t1,t2,tenv)
      } finally { depthbudget += 1; subtyping = save }
    }
    def tcast(t1: Typ, t2: Typ, tenv: TEnv): Typ = { assert(tsub(t1,t2,tenv)); t2 }

    def dsub(t1: DObj, t2: DObj, tenv: TEnv): Boolean = {
      def ts2(t1: (Typ,Typ), t2: (Typ,Typ), tenv: TEnv): Boolean =
        tsub(t2._1,t1._1,tenv) && tsub(t1._2, t2._2,tenv)
      def ts3(t1: (String,Typ,Typ), t2: (String,Typ,Typ), tenv: TEnv): Boolean =
        tsub(t2._2,t1._2,tenv) && tsub(t1._3, tsubst(t2._3,List(t2._1),Var(t1._1)),tenv)

      (t1.ts.keys == t2.ts.keys) && (t1.ls.keys == t2.ls.keys) && (t1.ms.keys == t2.ms.keys) && 
      (t1.ts.keys.forall(k => ts2(t1.ts(k),t2.ts(k),tenv))) &&
      (t1.ls.keys.forall(k => tsub(t1.ls(k),t2.ls(k),tenv))) &&
      (t1.ms.keys.forall(k => ts3(t1.ms(k),t2.ms(k),tenv)))
    }



    // type substitution
    def tsubstp(t: Exp, a: List[String], b: Exp): Exp = {  /*println(s"tsubstp $t $a -> $b");*/ t} match {
      //case Var(s)     => if (a contains s) b else Var(s)
      //case Sel(x,l) if b.isInstanceOf[Var]  => Sel(tsubstp(x,a,b),l)
      case _ => Subst(t,a,b)
    }
    def tsubstpFull(t: Exp, a: List[String], b: Exp): Exp = {  /*println(s"tsubstp $t $a -> $b");*/ t} match {
      case Var(s)     => if (a contains s) b else Var(s)
      case Sel(x,l)   => Sel(tsubstpFull(x,a,b),l)
      case Subst(x,u,v) => tsubstpFull(tsubstpFull(x,u,v),a,b)
    }
    def tsubst(t: Typ, a: List[String], b: Exp): Typ = {  /*println(s"tsubst $t $a -> $b");*/ t} match {
      case TTop                 => TTop
      case TBot                 => TBot
      case TAnd(x,y)            => TAnd(tsubst(x,a,b),tsubst(y,a,b))
      case TOr(x,y)             => TOr(tsubst(x,a,b),tsubst(y,a,b))
      case TSel(e,l)            => TSel(tsubstp(e,a,b),l)

      case TStruct(y,ts,ls,ms)  => 
        TStruct(y,
          ts.map(p=>p._1->((tsubst(p._2._1,a,b),tsubst(p._2._2,a,b)))), 
          ls.map(p=>p._1->tsubst(p._2,a,b)), 
          ms.map(p=>p._1->((p._2._1,tsubst(p._2._2,a,b),tsubst(p._2._3,a,b))))) 
  
      case TRef(x)              => TRef(tsubstp(x,a,b))
      case TObj(n,x)            => if (a contains n) TRef(b) else TObj(n,x) // should subst ??
    }


    // *** test cases

    def run(testProg: Exp) = {
      println("----")
      println("*** prog: " + prettyExp(testProg))
      store = Map()
      val res = teval(testProg, Map())
      println("*** tp: " + prettyTyp(res._1))
      //store.printBounds
      res
    }

/*
val b = new Any { zb =>
  type Foo = ...
  type Inner = Any { zi => type Bar; val z: zi.Bar ^ zb.Foo }
  val x: zb.Foo
}{}

b.x --> : b.Foo

b.Inner --> Any { zi => type Bar; val z: zi.Bar ^ b.Foo }

val a = new b.Inner { zf => ... } {z = ...}    :  let self = a in b.Inner

a.z --> : a.Bar ^ b.Foo
*/


/*
path equality
*/
    val testProg1 = 
      Let("b", New(TStruct("zb", 
        Map(
          "Foo"   -> (TBot,TTop),
          "Inner" -> (TBot,TStruct("zi",
              Map("Bar" -> (TBot,TTop)), 
              Map("z"   -> TAnd(TSel(Var("zi"), "Bar"), TSel(Var("zb"), "Foo"))), 
              Map()))
        ), Map(
          "x" -> TSel(Var("zb"), "Foo")
        ),Map()), Map(/*ls*/), Map(/*ms*/)),

      Let("a", New(TAnd(TSel(Var("b"), "Inner"), TStruct("zf", Map(), Map("self" -> TRef(Var("zf"))), Map())), Map(), Map()),
      Let("c", Var("a"),
        Sel(Sel(Var("c"), "self"), "z")
      )))

    val testProg2 = 
      Let("b", New(TStruct("zb", 
        Map(
          "Foo"   -> (TBot,TTop),
          "Inner" -> ((TBot,TStruct("zi",
              Map("Bar"      -> (TBot,TTop), 
                  "outerFoo" -> (TSel(Var("zb"), "Foo"),TSel(Var("zb"), "Foo"))), 
              Map(),
              Map("foo"      -> ("x", TTop, TSel(Var("zb"), "Foo"))))))
        ), Map(
          "x" -> TSel(Var("zb"), "Foo")
        ),Map()), Map(/*ls*/), Map(/*ms*/)),

      Let("a", New(TAnd(TSel(Var("b"), "Inner"), TStruct("zf", Map(), Map("self" -> TRef(Var("zf"))), Map())), Map(), Map()),
      Let("c", Var("a"),
        Let("r", App(Sel(Var("c"), "self"), "foo", Var("a")),
          Var("r")
      ))))

/*
expansion -- simple case
*/
    val testProg3 = Let("z", New(TStruct("zz", Map("L" -> (TBot,TSel(Var("zz"), "L"))), Map(), Map()), Map(), Map()),
      Var("z"))

/*
6.2.1: expansion, transitivity of <:
*/

    val testProg4 = 
      Let("u", New(TStruct("zu", 
        Map(
          "Bad"     -> (TBot,TSel(Var("u"), "Bad")), 
          "Good"    -> (TStruct("z1", Map("L" -> (TBot,TTop)), Map(), Map()), TStruct("z1", Map("L" -> (TBot,TTop)), Map(), Map())),
          "Lower"   -> (TAnd(TSel(Var("u"), "Bad"), TSel(Var("u"), "Good")), TSel(Var("u"), "Good")),
          "Upper"   -> (TSel(Var("u"), "Good"), TOr(TSel(Var("u"), "Bad"), TSel(Var("u"), "Good"))),
          "X"       -> (TSel(Var("u"), "Lower"), TSel(Var("u"), "Upper"))
        ), Map(), Map()),Map(),Map()),
        Var("u"))

    val test4TP = Map(
          "S"       -> TAnd(TSel(Var("u"),"Bad"),TSel(Var("u"),"Good")),
          "T"       -> TSel(Var("u"),"Lower"),
          "U"       -> TAnd(TSel(Var("u"),"X"),TStruct("z3",Map("L"->(TBot,TTop)),Map(),Map()))
        )

/*
dep method type
*/

    val testProg5 = 
      Let("b", New(TStruct("zb", 
        Map(
          "Foo"   -> (TBot,TTop),
          "Inner" -> ((TBot,TStruct("zi",
              Map("Bar"      -> (TBot,TTop), 
                  "outerFoo" -> (TSel(Var("zb"), "Foo"),TSel(Var("zb"), "Foo"))), 
              Map("obj"      -> (TSel(Var("zi"), "Bar"))),
              Map("foo"      -> ("x", TSel(Var("zb"),"Inner"), TSel(Var("x"), "Bar"))))))
        ), Map(
          "x" -> TSel(Var("zb"), "Foo")
        ),Map()), Map(/*ls*/), Map(/*ms*/)),

      Let("a", New(TAnd(TSel(Var("b"), "Inner"), TStruct("zf", Map(), Map("self" -> TRef(Var("zf"))), Map())), Map(), Map()),
      Let("c", Var("a"),
        Let("r", App(Sel(Var("c"), "self"), "foo", Sel(Var("a"),"self")),
          Var("r")
      ))))

/*
expansion case

val a = new { za =>
  C : Bot .. { c =>
    M : c.f.M .. c.f.M
    f : za.C
  }
} ( .. )
val b = new a.C ( .. )

expand b.M  -->  b.f.f...f.f.M

*/

    val testType6 = TStruct("za", 
        Map(
          "C" -> ((TBot,TStruct("c",
              Map("M"      -> ((TSel(Sel(Var("c"), "f"), "M"),TSel(Sel(Var("c"), "f"), "M")))), 
              Map("f"      -> (TSel(Var("za"), "C"))),
              Map())))),
        Map(), Map())

    /*
    val tenv6 = Map("a" -> testType6, "b" -> TSel(Var("a"), "C"))
    println(texpand(TSel(Var("b"), "M"), Var("z"), tenv6))
    */

/*
    env = { a -> \za. { C = \c. { M = c.f.M,  f = za.C }},  b -> a.C }

    b.M 
    (b: a.C).M
    (b: (a: { za => C = { c => M = c.f.M,  f = za.C }}).C).M
    (b: { c => M = c.f.M,  f = [za->a].C }).M
    [c->b].f.M

    ([c->b]: a.C).f.M
    ([c->b]: (a: { za => C = { c => M = c.f.M,  f = za.C }}).C).f.M
    ([c->b]: { c => M = c.f.M,  f = [za->a].C }).f.M
    ([c->[c->b]].f: [za->a].C ).M  

    // first opportunity to bail out: 
    // c->RHS, and we detect c in RHS (but final result is b, so ok)
    
    ([c->[c->b]].f: (a: { za => C = { c => M = c.f.M,  f = za.C }}).C) ).M
    ...
    ([c->[c->b]].f: { c => M = c.f.M,  f = [za->a].C }.M

    [c -> [c->[c->b]].f].f.M  

    // definitely stop here: c = c.f
*/

  }

  def testA = withOutFileChecked(prefix+"A") {
    import Test1._

    Test1.run(testProg1)
    val (t2,tenv2) = Test1.run(testProg2)
    println("a.self <: b.Inner (true)")
    println(tsub(TRef(Sel(Var("a"),"self")), TSel(Var("b"),"Inner"), tenv2))
    println("b.Inner <: a.self (false)")
    println(tsub(TSel(Var("b"),"Inner"), TRef(Sel(Var("a"),"self")), tenv2))
    println("a.self <: c (true)")
    println(tsub(TRef(Sel(Var("a"),"self")), TRef(Var("c")), tenv2))
    println("c <: a.self (true)")
    println(tsub(TRef(Var("c")), TRef(Sel(Var("a"),"self")), tenv2))
    println("a.outerFoo <: b.Foo (true)")
    println(tsub(TSel(Var("a"), "outerFoo"), TSel(Var("b"),"Foo"), tenv2))
    println("b.Foo <: a.outerFoo (true)")
    println(tsub(TSel(Var("b"),"Foo"), TSel(Var("a"), "outerFoo"), tenv2))

    val (t3,tenv3) = Test1.run(testProg3)
    println("expand z.L")
    println(texpand(TSel(Var("z"),"L"), Var("?"), tenv3))
    
    val (t4,tenv4) = Test1.run(testProg4)
    println(test4TP)
    println("S<:T (true)")
    println(tsub(test4TP("S"),test4TP("T"),tenv4))
    println("T<:U (true)")
    println(tsub(test4TP("T"),test4TP("U"),tenv4))
    println("S<:U (true)")
    println(tsub(test4TP("S"),test4TP("U"),tenv4))

    println("U<:S (false)")
    println(tsub(test4TP("U"),test4TP("S"),tenv4))

    val (t5,tenv5) = Test1.run(testProg5)
    println("should have r: a.Bar = c.Bar (true)")
    println(tsub(TRef(Var("r")), TSel(Sel(Var("c"),"self"), "Bar"), tenv5))

    println("expansion with cyclic paths: should have a 'C not found' error")
    val tenv6 = Map("a" -> testType6, "b" -> TSel(Var("a"), "C"))
    println(texpand(TSel(Var("b"), "M"), Var("z"), tenv6))

  }

/*
  questions/differences: 
    named types (TObj) and singleton-ref types (TRef); 
    dependent method types
    expand types always wrt to a self-path
    treat non-expanding types as expanding to empty set of decls
    express refinement as intersection with struct type (ok with self type?)
*/

}