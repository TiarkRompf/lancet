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

class TestAnalysis1 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-1"

/* 
    generalizing the store abstraction

    (general order of optimizations:)
      1) rewrite to constant (3+1 --> 4; x*0 --> 0)
      2) rewrite to previously defined value (x+0 --> x; x+y-y --> x)
      3) don't perform operation twice (cse)


    (requirements:)
      1) forward analysis to shortcut and eliminate
         reads (propagate values through data structures)
      2) generate dependencies between statements,
         so that they can be soundly moved around
         and dce'd (enable backwards analysis on dep 
         graph)

    concrete semantics:

      Frame = Local -> Val (Frame not used here)
      Val   = Prim Int | Ref Addr
      Obj   = Class x Field -> Val
      Store = Addr -> Obj

      eval: Exp => Val
      e[ new C ]            =   { val a        = freshAddr
                                  store        = store update [a -> (C, fields(C) -> null))]
                                  a }
      e[ x.f = y ]          =   { val Ref a    = eval(x)
                                  val y'       = eval(y) 
                                  val C,fields = store(a)
                                  store        = store update [a -> (C, fields update [f -> y'])]
                                  () }
      e[ x.f ]              =   { val Ref a    = eval(x)
                                  val C,fields = store(a)
                                  fields(f) }
      e[ if (c) a else b ]  =   { val Prim c'  = eval(x)
                                  if (c' != 0) eval(a) else eval(b) }

      (threading of store elided)


    collecting/abstract semantics:

      e[ if (c) a else b ]  =   { val Prim c' = eval(x)
                                  if (!mayZero(x')) 
                                    eval(a) 
                                  else if (mustZero(c')) 
                                    eval(b) 
                                  else eval(a) join eval(b) }

      Val^   = Prim Int^ | Ref Addr^
      Obj^   = Field -> Val^
      Store^ = Addr -> Obj^

      so: need to define Int^ and Addr^

        Int^:  mustZero, mayZero
        Addr^: 

      store.apply and store.update now
      need to take Addr^ objects

      need to precise what to update: we want
      strong updates -- only possible if we know
      that the target is a singleton

*/

  object Test1 {
    // concrete evaluation semantics

    type Addr = Int
    type Field = String

    abstract class Val
    case class Prim(x: Int) extends Val
    case class Ref(x: Addr) extends Val

    type Obj = Map[Field,Val]
    type Store = Map[Addr,Obj]

    abstract class Exp
    case class Const(x: Int) extends Exp
    case class Static(x: Int) extends Exp
    case class New() extends Exp
    case class Get(x: Exp, f: Field) extends Exp
    case class Put(x: Exp, f: Field, y: Exp) extends Exp
    case class If(c: Exp, a: Exp, b: Exp) extends Exp
    case class Block(xs: List[Exp]) extends Exp {
      override def toString = "{\n  " + xs.map(_.toString).mkString("\n").replace("\n","\n  ") + "\n}"
    }

    var store: Store = Map(0 -> Map.empty)

    var curAddr = 100
    def freshAddr() = { try curAddr finally curAddr += 1 }

    def eval(e: Exp): Val = e match {
      case Const(x) => Prim(x)
      case Static(x) => Ref(x)
      case If(c,a,b) => 
        val Prim(c1) = eval(c)
        if (c1 != 0) eval(a) else eval(b)
      case Block(xs) => xs map eval reduceLeft ((a,b) => b)
      case New() => 
        val a = freshAddr()
        store = store + (a -> Map.empty)
        Ref(a)
      case Get(x, f) => 
        val Ref(a) = eval(x)
        store(a)(f)
      case Put(x, f, y) => 
        val Ref(a) = eval(x)
        val y1 = eval(y)
        val x1 = store.getOrElse(a, Map.empty)
        store = store + (a -> (x1 + (f -> y1)))
        Prim(0)
    }


    val testProg1 = Block(List(
      Put(Static(0), "counter", Const(1)),
      If(Get(Static(0), "counter"),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "foo", Const(5))
        )),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "bar", Const(5))
        ))
      )
    ))

    def run() = {
      println("prog: " + testProg1)
      val res = eval(testProg1)
      println("res: " + res)
      println("store: " + store)
    }

  }

  object Test2 {
    // abstract semantics with sets as values
    // 'may' style information; for example loses 
    // information about undefined locations

    type Addr = Int
    type Field = String

    type AbsInt = Set[Int]
    type AbsAddr = Set[Addr]

    def mayZero(x: AbsInt) = x contains 0
    def mustZero(x: AbsInt) = x == Set(0)

    abstract class Val {
      def join(that: Val) = (this,that) match {
        case (Prim(x), Prim(y)) => Prim(x ++ y)
        case (Ref(x), Ref(y)) => Ref(x ++ y)
      }
    }
    case class Prim(x: AbsInt) extends Val
    case class Ref(x: AbsAddr) extends Val

    type Obj = Map[Field,Val]
    type Store = Map[Addr,Obj]

    def objJoin(a: Obj, b: Obj): Obj = {
      val m = (a.keys ++ b.keys).map(k => k -> ((a.get(k),b.get(k)) match {
        case (Some(u),Some(v)) => u join v
        case (Some(u),_) => u
        case (u,Some(v)) => v
      }))
      m.toMap
    }
    def objJoin(os: List[Obj]): Option[Obj] =
      if (os.isEmpty) None else Some(os.reduceLeft(objJoin))

    abstract class AbsStore { self =>
      override def toString = List.range(0,1000).flatMap(a=>get(Set(a)).map(a -> _)).mkString("\n")
      def getOrElse(a: AbsAddr, b: => Obj): Obj = get(a).getOrElse(b)
      def apply(a: AbsAddr): Obj = get(a).get
      def get(a: AbsAddr): Option[Obj]
      def +(p: (AbsAddr,Obj)): AbsStore = join(AbsStore(p._1.map(_ -> p._2).toMap))
      def join(other: AbsStore): AbsStore = new AbsStore {
        def get(a: AbsAddr): Option[Obj] =
          objJoin(self.get(a).toList ++ other.get(a).toList)
      }
    }

    object AbsStore {
      def apply(x: Store) = new AbsStore {
        def get(a: AbsAddr): Option[Obj] =
          objJoin(a.toList.flatMap(x.get(_).toList))
      }
    }


    abstract class Exp
    case class Const(x: Int) extends Exp
    case class Static(x: Int) extends Exp
    case class New() extends Exp
    case class Get(x: Exp, f: Field) extends Exp
    case class Put(x: Exp, f: Field, y: Exp) extends Exp
    case class If(c: Exp, a: Exp, b: Exp) extends Exp
    case class Block(xs: List[Exp]) extends Exp {
      override def toString = "{\n  " + xs.map(_.toString).mkString("\n").replace("\n","\n  ") + "\n}"      
    }

    val store0: AbsStore = AbsStore(Map(0 -> Map("counter" -> Prim(Set(0,1,2,3,4,5,6,7,8,9)))))
    var store: AbsStore = _

    var curAddr = 100
    def freshAddr() = { try Set(curAddr) finally curAddr += 1 }

    def eval(e: Exp): Val = e match {
      case Const(x) => Prim(Set(x))
      case Static(x) => Ref(Set(x))
      case If(c,a,b) => 
        val Prim(c1) = eval(c)
        println("c "+c1)
        if (!mayZero(c1)) eval(a) else if (mustZero(c1)) eval(b) else {
          val save = store
          val e1 = eval(a)
          val s1 = store
          store = save
          val e2 = eval(b)
          val s2 = store
          store = s1 join s2
          e1 join e2
        }
      case Block(xs) => xs map eval reduceLeft ((a,b) => b)
      case New() => 
        val a = freshAddr()
        store = store + (a -> Map.empty)
        Ref(a)
      case Get(x, f) => 
        val Ref(a) = eval(x)
        store(a)(f)
      case Put(x, f, y) => 
        val Ref(a) = eval(x)
        val y1 = eval(y)
        val x1 = store.getOrElse(a, Map.empty)
        store = store + (a -> (x1 + (f -> y1)))
        Prim(Set(0))
    }


    val testProg1 = Block(List(
      //Put(Static(0), "counter", Const(1)),
      If(Get(Static(0), "counter"),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "foo", Const(5))
        )),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "bar", Const(5))
        ))
      )
    ))

    val testProg2 = Block(List(
      //Put(Static(0), "counter", Const(1)),
      Put(Static(1), "a", New()),
      Put(Get(Static(1), "a"), "baz", Const(3)),
      If(Get(Static(0), "counter"),
        Block(List(
          Put(Static(1), "a", New()), // strong update, overwrite
          Put(Get(Static(1), "a"), "foo", Const(5))
        )),
        Block(List(
          Put(Static(1), "a", New()), // strong update, overwrite
          Put(Get(Static(1), "a"), "bar", Const(5))
        ))
      ),
      Put(Get(Static(1), "a"), "bar", Const(7)) // this is not a strong update, because 1.a may be one of two allocs
    ))

    def run(testProg: Exp) = {
      println("prog: " + testProg)
      store = store0
      val res = eval(testProg)
      println("res: " + res)
      println("store: \n" + store)
    }
  }

  object Test3 {
    // symbolic abstract values.
    // can answer both may and must queries,
    // enabling strong updates

    type Addr = Int
    type Field = String

    abstract class AbsInt {
      def ++(x: AbsInt) = PhiInt(this,x)
    }
    case class UndefinedInt() extends AbsInt
    case class ConstInt(x:Int) extends AbsInt
    case class PhiInt(x:AbsInt,y:AbsInt) extends AbsInt

    def mayContain(x: AbsInt): Set[Int] = x match {
      case UndefinedInt() => Set()
      case ConstInt(x) => Set(x)
      case PhiInt(x,y) => mayContain(x) ++ mayContain(y)
    }
    def mustContain(x: AbsInt): Set[Int] = x match {
      case UndefinedInt() => Set()
      case ConstInt(x) => Set(x)
      case PhiInt(x,y) => mustContain(x) intersect mustContain(y)
    }
    def mayZero(x: AbsInt): Boolean = x match {
      case UndefinedInt() => false
      case ConstInt(0) => true
      case ConstInt(_) => false
      case PhiInt(x,y) => mayZero(x) || mayZero(y)
    }
    def mustZero(x: AbsInt): Boolean = x match {
      case UndefinedInt() => false
      case ConstInt(0) => true
      case ConstInt(_) => false
      case PhiInt(x,y) => mustZero(x) && mustZero(y)
    }

    abstract class AbsAddr {
      def ++(x: AbsAddr) = PhiAddr(this,x)
    }
    case class UndefinedAddr() extends AbsAddr
    case class ConstAddr(x:Addr) extends AbsAddr
    case class PhiAddr(x:AbsAddr,y:AbsAddr) extends AbsAddr

    def mayContain(x: AbsAddr): Set[Addr] = x match {
      case UndefinedAddr() => Set()
      case ConstAddr(x) => Set(x)
      case PhiAddr(x,y) => mayContain(x) ++ mayContain(y)
    }
    def mustContain(x: AbsAddr): Set[Addr] = x match {
      case UndefinedAddr() => Set()
      case ConstAddr(x) => Set(x)
      case PhiAddr(x,y) => mustContain(x) intersect mustContain(y)
    }

    abstract class Val {
      def toFlatString = this match {
        case Prim(x) => "must:"+mustContain(x)+"may:"+mayContain(x)
        case Ref(x) => "must:"+mustContain(x)+"may:"+mayContain(x)
      }
      def join(that: Val) = (this,that) match {
        case (Prim(x), Prim(y)) => Prim(x ++ y)
        case (Ref(x), Ref(y)) => Ref(x ++ y)
        case (Ref(UndefinedAddr()), Prim(y)) => Prim(UndefinedInt() ++ y) // HACK
        case (Prim(x), Ref(UndefinedAddr())) => Prim(x ++ UndefinedInt()) // HACK
      }
    }
    case class Prim(x: AbsInt) extends Val
    case class Ref(x: AbsAddr) extends Val

    type Obj = Map[Field,Val]

    abstract class AbsObj {
      def toFlatString = mayFields(this).map(f=>f->this(f).toFlatString).toMap.toString
      def apply(f: Field): Val = this match {
        case UndefinedObj() => Ref(UndefinedAddr()) // FIXME: might be addr or int
        case ConstObj(x) => x.getOrElse(f, Ref(UndefinedAddr())) // FIXME: might be addr or int
        case PhiObj(x,y) => x(f) join y(f)
        case UpdateObj(x,f2,y) if f == f2 => y
        case UpdateObj(x,f2,y) => x(f)
      }
      def +(p: (Field,Val)): AbsObj = this match {
        case ConstObj(x) => ConstObj(x + p)
        case UpdateObj(that, f, _) if f == p._1 => that + p
        case PhiObj(x,y) => objJoin(x + p, y + p)
        case _ => UpdateObj(this,p._1,p._2)
      }

    }
    case class UndefinedObj() extends AbsObj
    case class ConstObj(x:Obj) extends AbsObj
    case class PhiObj(x:AbsObj,y:AbsObj) extends AbsObj
    case class UpdateObj(x:AbsObj,f:Field,y:Val) extends AbsObj

    def mayFields(x: AbsObj): Set[Field] = x match {
      case UndefinedObj() => Set()
      case ConstObj(x) => x.keys.toSet
      case PhiObj(x,y) => mayFields(x) ++ mayFields(y)
      case UpdateObj(x,f,y) => mayFields(x) ++ Set(f)
    }

    type Store = Map[Addr,Obj]

    def objJoin(a: AbsObj, b: AbsObj): AbsObj = if (a == b) a else PhiObj(a,b)
    def objJoin(os: List[AbsObj]): Option[AbsObj] =
      if (os.isEmpty) None else Some(os.reduceLeft(objJoin))

    abstract class AbsStore { self =>
      override def toString = List.range(0,1000).flatMap(a=>get(ConstAddr(a)).map(a -> _)).mkString("\n")
      def toPartialString = List.range(0,1000).flatMap(a=>get(ConstAddr(a)).map(b => a -> mayFields(b).map(f => f -> b(f)).toMap)).mkString("\n")
      def toFlatString = List.range(0,1000).flatMap(a=>get(ConstAddr(a)).map(b => a -> b.toFlatString)).mkString("\n")
      def getOrElse(a: AbsAddr, b: => AbsObj): AbsObj = get(a).getOrElse(b)
      def apply(a: AbsAddr): AbsObj = get(a).get
      def get(a: AbsAddr): Option[AbsObj]
      def +(p: (AbsAddr,AbsObj)): AbsStore = new AbsStore {
        val (u,o) = p
        def get(a: AbsAddr): Option[AbsObj] =
          // TODO
          // concrete: if (a == u) o else self.get(a)
          // abstract: if (u.mustContain(a)) o else
          //           if (u.mayContain(a)) 
          (u,a) match {
            case (ConstAddr(x),ConstAddr(y)) => // strong update!
              if (x == y) Some(o) else self.get(a)
            case _ =>
              objJoin(self.get(a).toList ++ AbsStore(mayContain(u).map(_ -> o).toMap).get(a).toList)
          }
      }
      def join(other: AbsStore): AbsStore = new AbsStore {
        def get(a: AbsAddr): Option[AbsObj] =
          objJoin(self.get(a).toList ++ other.get(a).toList)
      }
    }

    object AbsStore {
      def apply(x: Map[Addr,AbsObj]) = new AbsStore {
        def get(a: AbsAddr): Option[AbsObj] = {
          val as = mayContain(a)
          objJoin(as.toList.flatMap(x.get(_).toList))
        }
      }
    }



    abstract class Exp
    case class Const(x: Int) extends Exp
    case class Static(x: Int) extends Exp
    case class New() extends Exp
    case class Get(x: Exp, f: Field) extends Exp
    case class Put(x: Exp, f: Field, y: Exp) extends Exp
    case class If(c: Exp, a: Exp, b: Exp) extends Exp
    case class Block(xs: List[Exp]) extends Exp {
      override def toString = "{\n  " + xs.map(_.toString).mkString("\n").replace("\n","\n  ") + "\n}"      
    }

    val store0: AbsStore = AbsStore(Map(0 -> ConstObj(Map("counter" -> 
      Prim(PhiInt(ConstInt(0),PhiInt(ConstInt(1),PhiInt(ConstInt(2),PhiInt(ConstInt(3),
        PhiInt(ConstInt(4),PhiInt(ConstInt(5),PhiInt(ConstInt(6),PhiInt(ConstInt(7),
          PhiInt(ConstInt(8),ConstInt(9))))))))))))),
      1 -> ConstObj(Map("a" -> Ref(UndefinedAddr())))))

    var store: AbsStore = _

    var curAddr = 100
    def freshAddr() = { try ConstAddr(curAddr) finally curAddr += 1 }

    def eval(e: Exp): Val = e match {
      case Const(x) => Prim(ConstInt(x))
      case Static(x) => Ref(ConstAddr(x))
      case If(c,a,b) => 
        val Prim(c1) = eval(c)
        println("c "+c1)
        if (!mayZero(c1)) eval(a) else if (mustZero(c1)) eval(b) else {
          val save = store
          val e1 = eval(a)
          val s1 = store
          store = save
          val e2 = eval(b)
          val s2 = store
          store = s1 join s2
          e1 join e2
        }
      case Block(xs) => xs map eval reduceLeft ((a,b) => b)
      case New() => 
        val a = freshAddr()
        store = store + (a -> ConstObj(Map.empty))
        Ref(a)
      case Get(x, f) => 
        val Ref(a) = eval(x)
        store(a)(f)
      case Put(x, f, y) => 
        val Ref(a) = eval(x)
        val y1 = eval(y)
        val x1 = store.getOrElse(a, UndefinedObj())
        store = store + (a -> (x1 + (f -> y1)))
        Prim(ConstInt(0))
    }


    val testProg1 = Block(List(
      //Put(Static(0), "counter", Const(1)),
      If(Get(Static(0), "counter"),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "foo", Const(5))
        )),
        Block(List(
          Put(Static(1), "a", New()),
          Put(Get(Static(1), "a"), "bar", Const(5))
        ))
      )
    ))

    val testProg2 = Block(List(
      //Put(Static(0), "counter", Const(1)),
      Put(Static(1), "a", New()),
      Put(Get(Static(1), "a"), "baz", Const(3)),
      If(Get(Static(0), "counter"),
        Block(List(
          Put(Static(1), "a", New()), // strong update, overwrite
          Put(Get(Static(1), "a"), "foo", Const(5))
        )),
        Block(List(
          Put(Static(1), "a", New()), // strong update, overwrite
          Put(Get(Static(1), "a"), "bar", Const(5))
        ))
      ),
      Put(Get(Static(1), "a"), "bar", Const(7)) // this is not a strong update, because 1.a may be one of two allocs
    ))

    def run(testProg: Exp) = {
      println("prog: " + testProg)
      store = store0
      val res = eval(testProg)
      println("res: " + res)
      println("store: \n" + store)
      println("partially flat store: \n" + store.toPartialString)
      println("flat store: \n" + store.toFlatString)
    }
  }

  // TODO: 
  // loops (finite set of addrs, need to analyze uniqueness)
  // abstract GC (we know the baz record is no longer accessible after the if -> remove it)
  // construct dependency graph

  // run it
  def testA = withOutFileChecked(prefix+"A") {
    Test1.run()
  }
  def testB = withOutFileChecked(prefix+"B") {
    Test2.run(Test2.testProg1)
    Test2.run(Test2.testProg2)
    println("imprecision due to lack of strong updates:")
    println("1.a still seems to have a baz field, but cannot!")
  }
  def testC = withOutFileChecked(prefix+"C") {
    Test3.run(Test3.testProg1)
    Test3.run(Test3.testProg2)
    println("strong updates: no baz field")
  }


}