package lancet.core

//import scala.virtualization.LIR.common._

import java.lang.reflect.Modifier

/*
trait Base_LIR extends Base {
  val IR: EffectExp

  type Rep[+T] = IR.Rep[T]
  type TypeRep[T] = Manifest[T]

  def infix_typ[T](x: Rep[T]): TypeRep[T]

}

trait Base_LIR2 extends Base_LIR {
  import IR._

  type Static[+T] = IR.Const[T]
  type Dyn[+T] = IR.Sym[T]

  val Static = IR.Const
  object Dyn {
    def apply
    def unapply[T](x: Rep[T]): Option[String] = x match {
      case Sym(n) => Some("x"+n)
      case _ => None
    }
  }

  def constToString(x:Any): String
*/


trait Base_LIR0 extends Base {
  def reflect[T:TypeRep](s: Any*): Rep[T]
  def reify[T](x: => Rep[T]): Block[T]

  def liftConst[T:TypeRep](x:T): Rep[T]

  def repManifest[T:Manifest]: Manifest[Rep[T]]

  type Block[+T]

  case class TypeRep[T](s: String) { override def toString = s }

  implicit def anyType[T:Manifest] = new TypeRep[T](manifestStr(manifest[T]))

  implicit object booleanType extends TypeRep[Boolean]("Boolean")
  implicit object byteType extends TypeRep[Byte]("Byte")
  implicit object charType extends TypeRep[Char]("Char")
  implicit object shortType extends TypeRep[Short]("Short")
  implicit object intType extends TypeRep[Int]("Int")
  implicit object longType extends TypeRep[Long]("Long")
  implicit object floatType extends TypeRep[Float]("Float")
  implicit object doubleType extends TypeRep[Double]("Double")
  implicit object objectType extends TypeRep[Object]("Object")

  implicit object unitType extends TypeRep[Unit]("Unit")

  def typeRep[T:TypeRep]: TypeRep[T] = implicitly[TypeRep[T]]


  var constantPool: Vector[AnyRef] = Vector.empty

  def constToString[T:TypeRep](x:T): String = x match {
    case x: Boolean => ""+x
    case x: Int => ""+x
    case x: Long => ""+x+"L"
    case x: Double => ""+x+"d"
    case x: Float => ""+x+"f"
    case x: Unit => "()"
    case null => "null"
    // TODO: primitives, arrays
    case s: String => ("\""+s.replace("\n","\\n")+"\"") // TODO: proper escape
    case c: Class[_] => 
      c.getName() match {
        case "char" => "classOf[Char]"
        case name => 
          ("Class.forName(\""+name+"\")")//("classOf["+c.getName+"]")
      }
    //case o: Array[Object] => ("(null:Array[Object])") // TODO
    //case o: Object => ("(null:"+o.getClass.getName+")")
    case _ => 
      var idx = constantPool.indexOf(x) // FIXME: use eq
      if (idx < 0) {
        constantPool = constantPool :+ x.asInstanceOf[AnyRef]
        idx = constantPool.size - 1
      }

      "CONST_" + idx
  }

  def classStr(x: Class[_]): String = if (x.isArray()) "Array["+classStr(x.getComponentType)+"]" else x.getName match {
    case "int" => "Int"
    case "byte" => "Byte"
    case "char" => "Char"
    case "long" => "Long"
    case "float" => "Float"
    case "double" => "Double"
    case "void" => "Unit"
    // FIXME
    case "lancet.core.Base_LIR$Rep" => "lancet.core.Base_LIR$Rep" // scalac complains 'no type params' -- huh??
    case "lancet.interpreter.TestInterpreter5$Decompiler" => "lancet.interpreter.TestInterpreter5#Decompiler" // scalac crash
    //TODO/FIXME
    case s if !Modifier.isPublic(x.getModifiers) => "Object /*" + s + "*/" //s <-- class may be private...
    case s if s.contains("$read$$") => "Object"
    case s => s
      //if (s.contains("$")) println("careful: "+s)
      val params = x.getTypeParameters
      if (params.length == 0) s
      else s + "[" + params.map(x=>"_").mkString(",") + "]"
  }

  def manifestStr(x: Manifest[_]) = classStr(x.erasure)

  def specCls(x: AnyRef): (AnyRef,Class[_]) = {
    val cls = x.getClass
    if (!Modifier.isPublic(cls.getModifiers)) return (x,classOf[Object])
    if (cls.getName.contains("$read$$")) return (x,classOf[Object])
    (x,cls)
  }



}

trait Base_LIR extends Base_LIR0 {

  //def constToString(x:Any): String

  def mirrorDef[A:TypeRep](d: Def[A], f: Transformer): Def[A] = d
  def mirror[A:TypeRep](d: Def[A], f: Transformer): Rep[A] = ???

  trait Traverser {

    def traverseBlock(b: Block[Any]): Unit = b match {
      case Block(stms,res) =>
        stms foreach traverseStm
    }

    def traverseStm(s: Stm): Unit = {
      s.blocks foreach traverseBlock
    }

  }


  trait Transformer {
    def apply[A](x: Rep[A]): Rep[A] = x
    def apply[A](b: Block[A]): Block[A] = transformBlock(b)
    def transformBlock[A](b: Block[A]): Block[A] = b
  }


  trait StructuralTransformer extends Transformer {

    override def transformBlock[A](b: Block[A]): Block[A] = b match {
      case Block(stms,res) =>
        Block(stms flatMap transformStm,res)
    }

    def transformStm(s: Stm): List[Stm] = s match {
      case ValDef(x,typ,rhs) => 
        List(ValDef(x,typ,rhs map {
          case b: Block[_] => transformBlock(b)
          case d: Def[a] => mirrorDef(d,this)(typ.asInstanceOf[TypeRep[a]])
          case e => e
        }))
      case Unstructured(s) => List(Unstructured(s))
    }
  }


  trait CodeGen extends Traverser {

    override def traverseBlock(b: Block[Any]): Unit = b match {
      case Block(List(ValDef("_",_,List(";",b:Block[_]))),_) => traverseBlock(b) // flatten block ...
      case _ => super.traverseBlock(b)
    }

    override def traverseStm(s: Stm): Unit = s match {
      case ValDef(x,typ,rhs) => 
        if (x != "_") Console.print("val "+x+" = ")
        rhs foreach {
          case b: Block[a] => 
            emitScalaBlock(b, this)(typ.asInstanceOf[TypeRep[a]])
          case d: Def[a] => 
            emitScala(d, this)(typ.asInstanceOf[TypeRep[a]])
          case e =>
            Console.print(e.toString)
        }
        Console.println()
      case Unstructured(s) => 
        Console.println(s)
    }

  }


  def quickString[A:TypeRep](d: Def[A]): String = d.toString


  def emitScala[A:TypeRep](d: Def[A], f: CodeGen): Unit = ???
  def emitScalaBlock[A:TypeRep](b: Block[A], f: CodeGen): Unit = {
    Console.println("{")
    f.traverseBlock(b)
    if (b.res != liftConst(())) Console.println(b.res)
    Console.print("}")
  }




  abstract class Rep[+T:TypeRep] { def typ: TypeRep[_] = implicitly[TypeRep[T]] }

  case class Static[+T:TypeRep](x: T) extends Rep[T] { // IR.Const
    // adding a type cast everywhere: TestC was having issues with literal 8 passed to Object param?
    override def toString = constToString(x) //if (x == null) "null" else constToString(x) + ".asInstanceOf[" + typ + "]" // skip null's type
  }

  case class Dyn[+T:TypeRep](s: String) extends Rep[T] { // extends IR.Exp
    override def toString = s 
  }

  case class Block[+T](stms: List[Stm], res: Rep[T]) { // IR.Block
    //override def toString = "{\n"+stms.mkString("\n")+"\n" + res + "\n}"
  }

  abstract class Def[+T]


  def fblocks(e: Any): List[Block[Any]] = e match {
    case b: Block[Any] => List(b)
    case p: Product => p.productIterator.toList.flatMap(fblocks(_))
    case _ => Nil
  }
  def fdeps(e: Any): List[Dyn[Any]] = e match {
    case s: Dyn[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(fdeps(_))
    case _ => Nil
  }


  abstract class Stm {
    def deps: List[Dyn[Any]]
    def blocks: List[Block[Any]]
  }

  case class ValDef[T](x: String, typ: TypeRep[T], rhs: List[Any]) /*rhs: Either[String,Rep[Any]]*/ extends Stm { // IR.TP with IR.Def
    // we shouldn't recurse in toString because things will get big
    // unfortunately cse still depends on it (need to move to node ids)
    override def toString = if (x == "_") rhs.mkString("") else "val "+x+/*":"+typ+*/" = "+rhs.mkString("") 
    def deps: List[Dyn[Any]] = fdeps(rhs)
    def blocks: List[Block[Any]] = fblocks(rhs)
  }

  case class Unstructured(s: String) extends Stm {
    override def toString = s
    def deps: List[Dyn[Any]] = Nil
    def blocks: List[Block[Any]] = Nil
  }


  def repManifest[T:Manifest]: Manifest[Rep[T]] = manifest[Rep[T]]

  var nSyms = 0
  def fresh = { nSyms += 1; "x" + (nSyms - 1) }

  def reflect[T:TypeRep](s: Any*): Rep[T] = { 
    val rhs = s.toString // FIXME: don't want to do this! (--> big?)

    (exprs.get(rhs) match { // cse?
      case Some(y) =>
        emitString("/* cse: "+rhs+" = "+ y + "*/")
        y
      case None =>
      if (typeRep[T] == typeRep[Unit]) {
        emit(ValDef("_", typeRep[T], s.toList)); liftConst(()).asInstanceOf[Rep[T]]
      } else {
        val x = fresh; 
        emit(ValDef(x,typeRep[T],s.toList)); 
        val y = Dyn[T](x)
        //FIXME: can't cse if stm has effects
        //FIXME: not everything is SSA (CONST_LUB) -- need to kill! (TODO: in emit if lhs not a fresh var)
        def isPure = !(rhs.contains("throw") || rhs.contains("new") || rhs.contains(".alloc") || rhs.contains(".put")) //HACK
        if (isPure)
          rewrite(rhs, y)
        y
      }
    }).asInstanceOf[Rep[T]]
  }
/*
  def reflect[T:TypeRep](s: Any*)(s: Summary): Rep[T] = {
    val rhs = s.mkString("")

    ({//exprs.getOrElse(rhs, {
      if (typeRep[T] == typeRep[Unit]) {
        emit(ValDef("_", s.toList)); liftConst(()).asInstanceOf[Rep[T]]
      } else {
        rewrite(rhs)
        val x = fresh; emit(ValDef(x,s.toList)); Dyn[T](x)
      }
    }).asInstanceOf[Rep[T]]
  }
*/


  var exprs: Map[String, Rep[Any]] = Map.empty // maps
  //var envdef: Map[String, ValDef[T]] = Map.empty

  def rewrite(s: String, x: Rep[Any]): Unit = {
    // not used yet
    //assert(false, "REWRITE not used yet")
    exprs += (s -> x)
  }

  //FIXME: need lub!




  //var d = 0
  var stms: List[Stm] = null

  def emit(s: Stm) = {
    //System.out.println("  "*d + s)
    stms = s::stms
  }


  def reify[T](x: => Rep[T]): Block[T] = {
    val save = stms
    stms = Nil
    try {
      //System.out.println("  "*d + "<<")
      //d += 1
      val res = x
      //d -= 1
      //System.out.println("  "*d + ">>")
      Block(stms.reverse,res)
    } finally {
      stms = save
    }
  }


  def emitString(s: String) = emit(Unstructured(s.toString))

  def emitBlock(s: Block[Any]) = assert(false) // TODO

  //def println(s: Any) = assert(false)

  def captureOutput[A](func: => Rep[A]): String = {
    val (s,r) = captureOutputResult(func)
    s + "\n" + r
  }
  def captureOutputResult[A](func: => Rep[A]): (String,Rep[A]) = {
    val Block(stms,res) = reify(func)
    (stms.mkString("\n"),res)
  }


  import java.io._
  def captureConsoleOutputResult[A](func: => A): (String,A) = {
    val bstream = new ByteArrayOutputStream
    val r = withConsoleOutput(new PrintStream(bstream))(func) //func
    (bstream.toString, r)
  }
  def withConsoleOutput[A](out: PrintStream)(func: => A): A = {
    //val oldStdOut = System.out
    //val oldStdErr = System.err
    try {
      //System.setOut(out)
      //System.setErr(out)
      scala.Console.withOut(out)(scala.Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      //System.setOut(oldStdOut)
      //System.setErr(oldStdErr)
    }
  }



}



trait Base_LIR_Abs extends Base {

  abstract class Val[+T]
  case class Const[+T](x: T) extends Val[T] { override def toString = ("Const("+x+")").replace("\n","\\n") }
  case class Partial[+T](fields: Map[String, Rep[Any]]) extends Val[T]
  case object Top extends Val[Nothing]

  def eval[T](x: Rep[T]): Val[T]


}

//trait Base_Opt extends Base_LIR_Opt

trait Base_LIR_Opt extends Base_LIR_Abs with Base_LIR {

  object ExprLattice {
    type Elem = Map[String, Rep[Any]]
    def bottom: Elem = Map.empty
    // x is 'target' elem, y is 'current' elem
    def lub(x: Elem, y: Elem): Elem = {
      val ks = x.keys.toSet intersect y.keys.toSet
      val r1 = ks.map { k =>
        (k, (x.get(k), y.get(k)) match {
          case (Some(a),Some(b)) if a == b => Some(a)
          case _ => None
        })
      }
      val r2 = r1.collect{case (k,Some(v)) => (k,v)}.toMap
      r2
    }
    def getFields(x: Elem): Set[Rep[Any]] = { // only unique aliases
      x.values.toSet
    }
  }


  var store: StoreLattice.Elem = Map.empty

  // strategy for static values:
  // - put into the store only when written
  // - this means after if () {a.x = y} else {}
  //   only one branch may have the object in the store.
  //   taking the lub needs to account for this.
  //   (lub should not be Top!)

  def eval[T](e: Rep[T]): Val[T] = e match {
    //case Static(x) => Const(x)
    case Static(x) => store.get(constToString(x)(e.typ.asInstanceOf[TypeRep[T]])).asInstanceOf[Option[Val[T]]] match {
      case Some(x) => x
      case None => Const(x)
    }
    case Dyn(s) => store.getOrElse(s, Top).asInstanceOf[Val[T]] match {
      case x => x
    }
    case _ => 
      emitString("ERROR // can't eval: " + e)
      Top
  }

  // TODO: generalize and move elsewhere
  
  def getFieldForLub[T:TypeRep](base: Rep[Object], cls: Class[_], k: String): Rep[T] = throw new Exception

  object StoreLattice {
    type Elem = Map[String, Val[Any]]
    def bottom: Elem = Map.empty
    def getAllocs(x: Elem): Set[String] = x.collect { case (k,Partial(as)) => k }.toSet

    def getFields(x: Elem): Set[Rep[Any]] = { // only unique aliases
      x.values.collect { case Partial(as) => as.values } .flatten.toSet
    }

    def getDynFields(x: Elem): Set[Rep[Any]] = getFields(x).collect { case s@Dyn(_) => s }

    def getAllRefs(x: Elem) = getAllocs(x) ++ getFields(x).collect { case Dyn(s) => s }

    // x is 'target' elem, y is 'current' elem
    def lub(x: Elem, y: Elem): Elem = {

      // TODO: lub partials

      //def lubRep[A](a: Rep[A])
/*
      def lubVal[A](a: Val[A], b: Val[A]): Val[A] = (a,b) match {
        case (a,Top) => Top
        case (Top,b) => Top
        case (Const(u), Const(v)) if u == v => Const(u)
        case _ => Top
      }
*/

      /*
  
      TODO: - need to handle SSA-like fields in partial objects
            - we create new LUB_parent_key entries
            - when calling a block, we need to set up the right
              references on the calling side and in the parameter list
      */


      type PElem = Map[String, Rep[Any]]

      val y0 = y

      def lubPartial(p: String)(x: PElem, y: PElem): PElem = {
        val ks = x.keys ++ y.keys
        ks.map { k =>
          (k, (x.get(k), y.get(k)) match {
            case (Some(a),Some(b)) if a == b => a
            case (Some(Static(a)),Some(bb@Static(b))) => 
              val str = "LUB_"+p+"_"+k
              if (""+b != str)
                emitString("val "+str+" = " + b + " // LUBC(" + a + "," + b + ")") // FIXME: kill in expr!
              val tp = bb.typ.asInstanceOf[TypeRep[Any]]
              Dyn[Any](str)(tp)
            case (Some(a),None) if p.startsWith("CONST") && k == "clazz" => a // class is constant
            case (None,Some(b)) if p.startsWith("CONST") && k == "clazz" => b // class is constant
            case (a,b) => 
              val str = "LUB_"+p+"_"+k
              val tp = if (b.nonEmpty) {
                //if (b.get.toString != str && !y0.contains(b.get.toString)) {
                //  emitString("// PROBLEMO "+b.get+" not in "+y0)
                //}
                if (b.get.toString != str)
                  emitString("val "+str+" = " + b.get + " // Alias(" + a + "," + b + ")") // FIXME: kill in expr!
                b.get.typ.asInstanceOf[TypeRep[Any]]
              } else {                
                val tp = a.get.typ.asInstanceOf[TypeRep[Any]]
                // we don't have the b value in the store
                // check if this refers to a const field; if so get the field value
                if (p.startsWith("CONST")) {
                  val obj = y("alloc").asInstanceOf[Rep[Object]]
                  val cls:Class[_] = obj match { case Static(o) => o.getClass }
                  val fld = getFieldForLub(obj,cls,k)(tp)
                  //println("// lookup "+obj+"."+k+"="+fld)
                  // may fld and a.get be equal? unlikely ...
                  if (fld.toString != str)
                    emitString("val "+str+" = " + fld + " // XXX LUBC(" + a + "," + b + ")") // FIXME: kill in expr!
                } else 
                  emitString("val "+str+" = " + a.get + " // AAA Alias(" + a + "," + b + ")") // FIXME: kill in expr!
                tp
              }
              Dyn[Any](str)(tp)
          })
        }.toMap
      }

      val ks = x.keys ++ y.keys

      val r1 = ks.map { k =>
        (k, (x.get(k), y.get(k)) match {
          case (Some(a),Some(b)) if a == b => a
          case (Some(Partial(as)),Some(Partial(bs))) => Partial(lubPartial(k)(as,bs)) // two definitions ...
          // const: lift on the other side if missing; todo: replace string check with as("alloc") check
          case (Some(Partial(as)),None) if k.startsWith("CONST") => Partial(lubPartial(k)(as,Map("alloc"->as("alloc"),"clazz"->as("clazz")))) // final fields ...
          case (None,Some(Partial(bs))) if k.startsWith("CONST") => Partial(lubPartial(k)(Map("alloc"->bs("alloc"),"clazz"->bs("clazz")),bs))
          // allocs: may be null in alternative
          //case (Some(Partial(as)),None) => Partial(lubPartial(k)(as,Map("alloc"->liftConst(null))))
          //case (None,Some(Partial(bs))) => Partial(lubPartial(k)(Map("alloc"->liftConst(null)),bs))
          //case (Some(Partial(as)),None) => emitString("val "+k+" = null // lub "+Partial(as)+", None "); Top
          case (Some(a),Some(b)) => Top
          case (Some(a),b) => 
            //println("// strange lub: "+k+" -> Some("+a+"), "+b); 
            Top //a
          case (a,Some(b)) => 
            //println("// strange lub: "+k+" -> "+a+",Some("+b+")"); 
            Top //b
        })
      }

      val removed = r1.collect { case (k,Top) => k }

      //if (removed.nonEmpty)
      //  emitString("//removed: "+removed)

      val r2 = r1.filter(_._2 != Top).toMap

      //if (removed.exists(e => r2.toString.contains(e.toString)))
      //  println("// PROBLEMO "+r2+" contains "+removed)

      r2
    }

  }
}


