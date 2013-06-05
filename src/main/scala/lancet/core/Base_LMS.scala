package lancet.core

import scala.virtualization.lms.common._

import java.lang.reflect.Modifier
import scala.reflect.SourceContext

// *** core lms support classes

trait IR_LMS_Base extends EffectExp {

  def liftConst[T:TypeRep](x:T): Rep[T]
  type TypeRep[T]
  implicit def typeRepToManifest[T:TypeRep]: Manifest[T]
  
  def quickString[A:TypeRep](d: Def[A]): String = d.toString

  case class Unstructured[A](s: List[Any]) extends Def[A]
  case class Patch(key: String, var block: Block[Unit]) extends Def[Unit]
  case class BlockDef(key: String, keyid: Int, params: List[Rep[Any]], body: Block[Unit]) extends Def[Unit]

  // alternative idea: patches should not schedule stuff inside, but in
  // their parent scope. thus, do not override boundSyms for Patch,
  // but override effectsSyms to include effectSyms of all
  // patches...

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Unstructured(xs) => blocks(xs) flatMap effectSyms
    case Patch(key,block) => effectSyms(block)
    case BlockDef(key,keyid,xs,body) => effectSyms(body)// ++ List(phiSym)
    case _ => super.boundSyms(e)
  }

  lazy val phiSym = Sym(999)

  override def tunnelSyms(e: Any): List[Sym[Any]] = e match {
    case BlockDef(key,keyid,xs,body) => 
    // FIXME: here's a problem:
    // xs may contains items like DynExp("PHI_3_6") on which stuff in the block depends.
    // since these are not symbols we can't return them here....
    // (and dependent nodes might be hoisted to the top or removed)
    xs.collect { case s@Sym(n) => s }  ++ List(phiSym)//case d if d.toString.contains("PHI") => println(s"add accidental dependency $d: def x42"); phiSym }
    case _ => super.tunnelSyms(e)
  }


  override def syms(e: Any): List[Sym[Any]] = {
    val xx = super.syms(e)
    if (e.toString.contains("PHI")) { println(""+
      e + ":" + xx); xx ++ List(phiSym) } else xx
  }

  override def symsFreq(e: Any): List[(Sym[Any],Double)] = {
    val xx = super.symsFreq(e)
    if (e.toString.contains("PHI")) xx ++ List((phiSym,1.0)) else xx
  }


  override def mirrorDef[A:Manifest](d: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (d match {
    case d@Unstructured(xs)                   => Unstructured(xs.map{ case x: Exp[Any] =>f(x) case x => x})
    case d@Patch(key, block)                  => Patch(key,f(block))
    case d@BlockDef(key, keyid, params, body) => BlockDef(key,keyid,params.map(x=>f(x)),f(body))
    case _ => super.mirrorDef(d,f)
  }).asInstanceOf[Def[A]]


  def reflect[A:TypeRep](s: Any*): Exp[A] = reflectEffect(Unstructured[A](s.toList))

  def reflectPure[A:TypeRep](s: Any*): Exp[A] = toAtom(Unstructured[A](s.toList))


  def emitString(s: String)(implicit e:TypeRep[Unit]) = reflect[Unit](s)
  def emitAll[A:TypeRep](s: Block[A]) = reflect[A](s)
}

trait IR_LMS extends IR_LMS_Base

// *** code generation

trait GEN_Scala_LMS_Base extends ScalaGenEffect {
  val IR: Base_LMS
  import IR._

  def emitBlockFull(b: Block[Any]): Unit = {
    if (b.res == Const(())) stream.print("{ }") else {
    stream.println("{")
    emitBlock(b)
    if (getBlockResult(b).tp != manifest[Unit])
      stream.println(quote(getBlockResult(b)))
    stream.print("}")
  }}

  override def emitValDef(sym: Sym[Any], rhs: String) = 
    /*if (sym.tp == manifest[Unit]) stream.println(rhs+";")
    else*/ super.emitValDef(sym,rhs+";")

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Unstructured(xs) =>
        if (sym.tp != manifest[Unit])
          stream.print("val "+quote(sym)+": "+remap(sym.tp)+" = ")
        xs foreach {
          case b: Block[a] => 
            emitBlockFull(b)
          case b: Exp[a] => 
            stream.print(quote(b))
/*          case b: Manifest[a] => 
            stream.print(remap(b))
          case b: TypeRep[a] => 
            stream.print(remap(b.manif)) // ok?*/
          case e =>
            stream.print(e)
        }
        stream.println(";")
    case Patch(key, block) =>
    //stream.println("// patch "+sym+" {")
      emitBlock(block)
    //stream.println("// patch "+sym+" }")
    case BlockDef(key, keyid, params, body) =>
      if (debugBlockKeys) 
        stream.println("// "+key+"\n")
      stream.print("def BLOCK_"+keyid+"(")
      stream.print(params.map(v=>quote(v)+":"+remap(v.tp)).mkString(","))
      stream.print("): Unit = ")
      emitBlockFull(body)
      stream.println
    case _ => super.emitNode(sym,rhs)
  }

  override def remap[A](m: Manifest[A]): String = m match {
    case null => "NULL"
    case _ => super.remap(m)
  }


  override def quote(e: Exp[Any]) = e match {
    case DynExp(a) => a.toString
    case Const(a) => VConstToString(a)(e.typ)
    case Sym(n) if n <= -1000 => 
      // HACK: lms wants syms for consts subject to CSP
      "Const_"+(-n -1000)
    case _ => super.quote(e)
  }

  override def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = {
    VConstantPool.toList
  }
}

trait GEN_Scala_LMS extends GEN_Scala_LMS_Base with ScalaGenCore


// *** lancet -> lms interface

trait Base_LMS1 extends Base with IR_LMS { self =>
  //val IR: IR_LMS
  val IR: self.type = self

  // debug flags, move elsewhere?
  var debugBlockKeys = true

  //type Rep[+T] = IR.Rep[T]
  type TypeRep[T]// = Manifest[T]

  def infix_typ[T](x: Rep[T]): TypeRep[T]

  import IR._

  type Static[+T] = IR.Const[T]
  type Dyn[+T] = IR.Sym[T]

  case class DynExp[T:TypeRep](s: String) extends Exp[T]

  val Static = IR.Const
  object Dyn {
    def apply[T:TypeRep](s: String) = DynExp(s)
    def unapply[T](x: Rep[T]): Option[String] = x match {
      case DynExp(s) => Some(s)
      case Sym(n) => Some("x"+n)
      case _ => None
    }
  }
}


trait Base_LMS0 extends Base_LMS1 {
  def reflect[T:TypeRep](d: Def[T]): Rep[T]
  def reify[T:TypeRep](x: => Rep[T]): Block[T]

  def liftConst[T:TypeRep](x:T): Rep[T]

  def repManifest[T:Manifest]: Manifest[Rep[T]]

  type Block[+T]

  case class TypeRep[T:Manifest](s: String) { override def toString = s; val manif = manifest[T] }

  def typeRepToManifest[T:TypeRep]: Manifest[T] = typeRep[T].manif

  //implicit def anyType[T:Manifest] = new TypeRep[T](manifestStr(manifest[T]))

  val booleanManif = manifest[Boolean]
  val byteManif    = manifest[Byte]
  val charManif    = manifest[Char] 
  val shortManif   = manifest[Short]   
  val intManif     = manifest[Int] 
  val longManif    = manifest[Long] 
  val floatManif   = manifest[Float]   
  val doubleManif  = manifest[Double]   
  val objectManif  = manifest[Object]   

  val classManif   = manifest[Class[Object]]
  val unitManif    = manifest[Unit]
  val stringManif  = manifest[String]

  implicit object booleanType extends TypeRep[Boolean]("Boolean")(booleanManif)
  implicit object byteType    extends TypeRep[Byte   ]("Byte"   )(byteManif)
  implicit object charType    extends TypeRep[Char   ]("Char"   )(charManif)
  implicit object shortType   extends TypeRep[Short  ]("Short"  )(shortManif)
  implicit object intType     extends TypeRep[Int    ]("Int"    )(intManif)
  implicit object longType    extends TypeRep[Long   ]("Long"   )(longManif)
  implicit object floatType   extends TypeRep[Float  ]("Float"  )(floatManif)
  implicit object doubleType  extends TypeRep[Double ]("Double" )(doubleManif)
  implicit object objectType  extends TypeRep[Object ]("Object" )(objectManif)

  implicit object classType   extends TypeRep[Class[Object]]("Class[Object]" )(classManif)
  implicit object unitType    extends TypeRep[Unit   ]("Unit"   )(unitManif)
  implicit object stringType  extends TypeRep[String ]("String" )(stringManif)

  def typeRep[T:TypeRep]: TypeRep[T] = implicitly[TypeRep[T]]


  def quote(x:Any): String = x match {
    case Const(c:AnyRef) => VConstToString(c)(typeRep[AnyRef]) //hack?
    case Const(null) => VConstToString[AnyRef](null)(typeRep[AnyRef]) //hack?
    case Dyn(s) => s // x99
    case DynExp(x) => x
    case _ => x.toString
  }

  var VConstantPool: Vector[(Sym[Any],AnyRef)] = Vector.empty

  def VConstToString[T:TypeRep](x:T): String = x match {
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
      var idx = VConstantPool.indexWhere(_._2 == x) // should use eq ??
      if (idx < 0) {
        idx = VConstantPool.size
        VConstantPool = VConstantPool :+ ((Sym(-1000-idx)(typeRep[T].manif),x.asInstanceOf[AnyRef]))
      }

      "pConst_" + idx
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
    case "lancet.core.Base_LMS$Rep" => "lancet.core.Base_LMS$Rep" // scalac complains 'no type params' -- huh??
    case "lancet.interpreter.TestInterpreter5$Decompiler" => "lancet.interpreter.TestInterpreter5#Decompiler" // scalac crash
    //TODO/FIXME
    case s if !Modifier.isPublic(x.getModifiers) => "Object /*" + s + "*/" //s <-- class may be private...
    case s => 
      //if (s.contains("$")) println("careful: "+s)
      val params = x.getTypeParameters
      if (params.length == 0) s
      else s + "[" + params.map(x=>"_").mkString(",") + "]"
  }

  def manifestStr(x: Manifest[_]) = {
    val s = "" + x // NOTE: strangly, NPEs crop up if this line is removed?
    //println(s)
    classStr(x.erasure)
  }

  def specCls(x: AnyRef): (AnyRef,Class[_]) = {
    val cls = x.getClass
    if (Modifier.isPublic(cls.getModifiers)) (x,cls) else (x,classOf[Object])
    // for now, just fix to Object (hack?)
    if (cls == classOf[java.lang.reflect.Method]) (x,cls)
    else 
    (x,classOf[Object])
  }

  def isPrimitive[T:TypeRep](x: T) = x match {
    case x: Boolean => true
    case x: Int => true
    case x: Char => true
    case x: Short => true
    case x: Long => true
    case x: Float => true
    case x: Double => true
    case x: Unit => true
    case _ => false
  }
}

trait Base_LMS extends Base_LMS0 {

  def reflect[T:TypeRep](d: Def[T]): Rep[T] = reflectEffect(d)(typeRep[T].manif,implicitly[SourceContext])//toAtom(d)
  def reify[T:TypeRep](x: => Rep[T]): Block[T] = reifyEffects(x)(typeRep[T].manif)

  def reflectPure[T:TypeRep](d: Def[T]): Rep[T] = toAtom(d)(typeRep[T].manif,implicitly[SourceContext])


  def liftConst[T:TypeRep](x:T): Rep[T] = {
    //if (!isPrimitive(x))
      //VConstToString(x) // add to constant pool
    unit(x)(typeRep[T].manif)
  }

  def repManifest[T:Manifest]: Manifest[Rep[T]] = manifest[Rep[T]]

  def infix_typ[T](x: Rep[T]): TypeRep[T] = manifestToTypeRep(x.tp)

  def manifestToTypeRep[T](x: Manifest[T]): TypeRep[T] = TypeRep(manifestStr(x))(x)

}



trait Base_LMS_Abs extends Base {

  abstract class Val[+T]
  case class VConst[+T](x: T) extends Val[T] { override def toString = ("VConst("+x+")").replace("\n","\\n") }
  case class Partial[+T](fields: Map[String, Rep[Any]]) extends Val[T]
  case class VPhi[+T](c: Rep[Boolean], a: Rep[T], b: Rep[T]) extends Val[T]
  case object Top extends Val[Nothing]

  def eval[T](x: Rep[T]): Val[T]


}

//trait Base_Opt extends Base_LMS_Opt

trait Base_LMS_Opt extends Base_LMS_Abs with Base_LMS {

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

  var conds: CondLattice.Elem = Map.empty

  object CondLattice {
    type Elem = Map[Rep[Boolean],Boolean]
    def lub(x: Elem, y: Elem): Elem = {
      x ++ y // may/must ?
    }
  }


  var store: StoreLattice.Elem = Map.empty

  // strategy for static values:
  // - put into the store only when written
  // - this means after if () {a.x = y} else {}
  //   only one branch may have the object in the store.
  //   taking the lub needs to account for this.
  //   (lub should not be Top!)

  def eval[T](x: Rep[T]): Val[T] = x match {
    //case Static(x) => VConst(x)
    case Static(y) => store.get(VConstToString(y)(x.typ)).asInstanceOf[Option[Val[T]]] match {
      case Some(y) => y
      case None => VConst(y)
    }
    case Dyn(s) => store.getOrElse(s, Top).asInstanceOf[Val[T]] match {
      case x => x
    }
    case _ => 
      emitString("ERROR // can't eval: " + x)
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
        case (VConst(u), VConst(v)) if u == v => VConst(u)
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
              if (quote(b) != str)
                emitString("val "+str+" = " + quote(b) + "; // LUBC(" + a + "," + b + ")") // FIXME: kill in expr!
              val tp = bb.typ.asInstanceOf[TypeRep[Any]]
              Dyn[Any](str)(tp)
            case (Some(a),None) if p.startsWith("pConst") && k == "clazz" => a // class is VConstant
            case (None,Some(b)) if p.startsWith("pConst") && k == "clazz" => b // class is VConstant
            case (a,b) => 
              val str = "LUB_"+p+"_"+k
              val tp = if (b.nonEmpty) {
                //if (b.get.toString != str && !y0.contains(b.get.toString)) {
                //  emitString("// PROBLEMO "+b.get+" not in "+y0)
                //}
                if (quote(b.get) != str)
                  emitString("val "+str+" = " + quote(b.get) + "; // Alias(" + a + "," + b + ")") // FIXME: kill in expr!
                b.get.typ.asInstanceOf[TypeRep[Any]]
              } else {                
                val tp = a.get.typ.asInstanceOf[TypeRep[Any]]
                // we don't have the b value in the store
                // check if this refers to a VConst field; if so get the field value
                if (p.startsWith("pConst")) {
                  val obj = y("alloc").asInstanceOf[Rep[Object]]
                  val cls:Class[_] = obj match { case Static(o) => o.getClass }
                  val fld = getFieldForLub(obj,cls,k)(tp)
                  //println("// lookup "+obj+"."+k+"="+fld)
                  // may fld and a.get be equal? unlikely ...
                  if (quote(fld) != str)
                    emitString("val "+str+" = " + quote(fld) + "; // XXX LUBC(" + a + "," + b + ")") // FIXME: kill in expr!
                } else 
                  emitString("val "+str+" = " + quote(a.get) + "; // AAA Alias(" + a + "," + b + ")") // FIXME: kill in expr!
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
          // VConst: lift on the other side if missing; todo: replace string check with as("alloc") check
          case (Some(Partial(as)),None) if k.startsWith("pConst") => Partial(lubPartial(k)(as,Map("alloc"->as("alloc"),"clazz"->as("clazz")))) // final fields ...
          case (None,Some(Partial(bs))) if k.startsWith("pConst") => Partial(lubPartial(k)(Map("alloc"->bs("alloc"),"clazz"->bs("clazz")),bs))
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


