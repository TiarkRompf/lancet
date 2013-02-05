package lancet.core

trait Base_Str2 extends Base_Str {

  abstract class Rep[+T:TypeRep] { def typ: TypeRep[_] = implicitly[TypeRep[T]] }

  case class Static[+T:TypeRep](x: T) extends Rep[T] { 
    // adding a type cast everywhere: TestC was having issues with literal 8 passed to Object param?
    override def toString = constToString(x) + ".asInstanceOf[" + typ + "]"
  }

  case class Dyn[+T:TypeRep](s: String) extends Rep[T] {
    override def toString = s 
  }

  def repManifest[T:Manifest]: Manifest[Rep[T]] = manifest[Rep[T]]

  var nSyms = 0
  def fresh = { nSyms += 1; "x" + (nSyms - 1) }

  def emit(s: String) = println(s)//"          "+s)

  def reflect[T:TypeRep](s: Any*): Rep[T] = { 
    val rhs = s.mkString("")
    ({//exprs.getOrElse(rhs, {
      if (typeRep[T] == typeRep[Unit]) {
        emit(s.mkString("")); liftConst(()).asInstanceOf[Rep[T]]
      } else {
        val x = fresh; emit("val "+x+" = "+s.mkString("")); Dyn[T](x)
      }
    }).asInstanceOf[Rep[T]]
  }

  def reify[T](x: => Rep[T]): String = ("{\n" + indented(captureOutput(x)) + "\n}")
  // TODO: does reify need to worry about other state like the store?

}



trait Base_Opt_Abs extends Base {

  abstract class Val[+T]
  case class Const[+T](x: T) extends Val[T] { override def toString = ("Const("+x+")").replace("\n","\\n") }
  case class Partial[+T](fields: Map[String, Rep[Any]]) extends Val[T]
  case class Alias[+T](y: List[Rep[T]]) extends Val[T]
  case object Top extends Val[Nothing]

  def eval[T](x: Rep[T]): Val[T]



}


trait Base_Opt extends Base_Opt_Abs with Base_Str2 {

/*
  var exprs: Map[String, Rep[Any]] = Map.empty

  def rewrite(s: String, x: Rep[Any]): Unit = {
    // not used yet
    assert(false, "REWRITE not used yet")
    exprs += (s -> x)
  }
*/

/*
  new idea: need to track const modifications, too

  abstract class Addr
  abstract class Null
  abstract class Static(c)
  abstract class Alloc(site: String, mult: Int)  // site: finite unique id, mult: 1 single alloc/unique ref, 2 alloc in loop

  type Store = Map[Addr, AbsObj] // map Dyn ref to 
  type AbsObj = Map[String, Set[Rep[Any]]] // map field to value

  type Env = Map[String, Addr]
  def joinStore(a,b) = {  }
  def putObject(target, field) = {
    val (mstAddr, mayAddrs) = store(target)
    for (a <- mayAddrs diff mstAddr)
  }
 */




  var store: StoreLattice.Elem = Map.empty

  // strategy for static values:
  // - put into the store only when written
  // - this means after if () {a.x = y} else {}
  //   only one branch may have the object in the store.
  //   taking the lub needs to account for this.
  //   (lub should not be Top!)

  def eval[T](x: Rep[T]): Val[T] = x match {
    //case Static(x) => Const(x)
    case Static(x) => store.get(constToString(x)).asInstanceOf[Option[Val[T]]] match {
      case Some(x) => x
      case None => Const(x)
    }
    case Dyn(s) => store.getOrElse(s, Top).asInstanceOf[Val[T]] match {
      case Alias(List(x)) => eval(x) // TBD: cycles?
      case Alias(_) => Top // more than one alias
      case x => x
    }
    case _ => 
      println("ERROR // can't eval: " + x)
      Top
  }

  // TODO: generalize and move elsewhere
  
  def dealias[T](x: Rep[T]): Rep[T] = x match {
    case Dyn(s) => store.get(s) match {
        case Some(Alias(List(y:Rep[T]))) => dealias(y)
        case Some(Const(c:T)) => Static(c)(x.typ.asInstanceOf[TypeRep[T]])
        case _ => x
      }
    case _ => x
  }


  def getFieldForLub[T:TypeRep](base: Rep[Object], cls: Class[_], k: String): Rep[T] = throw new Exception

  object StoreLattice {
    type Elem = Map[String, Val[Any]]

    def bottom: Elem = Map.empty

    def getAllocs(x: Elem): Set[String] = x.collect { case (k,Partial(as)) => k }.toSet

    def getFields(x: Elem): Set[Rep[Any]] = { // only unique aliases
      x.values.collect { case Partial(as) => as.values case Alias(a::Nil) => a::Nil } .flatten.toSet
    }

    def getDynFields(x: Elem): Set[Rep[Any]] = getFields(x).collect { case s@Dyn(_) => s }


    def getAllRefs(x: Elem) = getAllocs(x) ++ getFields(x).collect { case Dyn(s) => s }


    def getLubParamArgs(x: Elem, y: Elem): Map[String, Rep[Any]] = {
      
      type PElem = Map[String, Rep[Any]]

      def lubPartial(p: String)(x: PElem, y: PElem): PElem = {
        val ks = x.keys ++ y.keys
        ks.flatMap { k =>
          ((x.get(k), y.get(k)) match {
            case (Some(Dyn(s:String)),Some(b)) /*if s.startsWith("LUB_")*/ => Map(s->b):PElem
            case _ => Map.empty:PElem
          })
        }.toMap
      }

      val ks = x.keys ++ y.keys

      ks.flatMap { k =>
        ((x.get(k), y.get(k)) match {
          case (Some(Partial(as)),Some(Partial(bs))) => (lubPartial(k)(as,bs))
          case _ => Map.empty:PElem
        })
      }.toMap
    }

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
                println("val "+str+" = " + b + " // LUBC(" + a + "," + b + ")")
              val tp = bb.typ.asInstanceOf[TypeRep[Any]]
              Dyn[Any](str)(tp)
            case (Some(a),None) if p.startsWith("CONST") && k == "clazz" => a // class is constant
            case (None,Some(b)) if p.startsWith("CONST") && k == "clazz" => b // class is constant
            case (a,b) => 
              val str = "LUB_"+p+"_"+k
              val tp = if (b.nonEmpty) {
                //if (b.get.toString != str && !y0.contains(b.get.toString)) {
                //  println("// PROBLEMO "+b.get+" not in "+y0)
                //}
                if (b.get.toString != str)
                  println("val "+str+" = " + b.get + " // Alias(" + a + "," + b + ")")
                b.get.typ.asInstanceOf[TypeRep[Any]]
              } else {                
                val tp = a.get.typ.asInstanceOf[TypeRep[Any]]
                // we don't have the b value in the store
                // check if this refers to a const field; if so get the field value
                if (p.startsWith("CONST")) {
                  val obj = y("alloc").asInstanceOf[Rep[Object]]
                  val cls:Class[_] = obj match { case Static(o) => o.getClass }
                  val fld = getFieldForLub(obj,cls,k)(tp)
                  println("// lookup "+obj+"."+k+"="+fld)
                  // may fld and a.get be equal? unlikely ...
                  if (fld.toString != str)
                    println("val "+str+" = " + fld + " // XXX LUBC(" + a + "," + b + ")")
                } else 
                  println("val "+str+" = " + a.get + " // AAA Alias(" + a + "," + b + ")")
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
          //case (Some(Partial(as)),None) => println("val "+k+" = null // lub "+Partial(as)+", None "); Top
          case (Some(Alias(as)),Some(Alias(bs))) => Alias(as ++ bs)
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

      if (removed.nonEmpty)
        println("//removed: "+removed)

      val r2 = r1.filter(_._2 != Top).toMap

      if (removed.exists(e => r2.toString.contains(e.toString)))
        println("// PROBLEMO "+r2+" contains "+removed)

      r2
    }


    def alpha(sto: Elem, from: List[Rep[Any]], to: List[Rep[Any]]): Elem = {

      val subst = (to zip from).toMap

/*      def alphaRep(x: Rep[Any]): Rep[Any] = x match {
        case Dyn(s) => subst.getOrElse(x, x)
        case _ => x
      }

      sto map { 
        case (k, Partial(fields)) => 
          subst get Dyn(k) match {
            case Some(Dyn)
          }
          (k, Partial(fields map (p => (p._1, alphaRep(p._2)))))
        case p => p
      }
*/
      
      def dealias[T](x: Rep[T]): Rep[T] = x match {
        case Dyn(s) => sto.get(s) match {
            case Some(Alias(List(y:Rep[T]))) => dealias(y)
            //case Some(Const(c:T)) => Static(c)(x.typ.asInstanceOf[TypeRep[T]])
            case _ => x
          }
        case _ => x
      }

      val sto2 = sto.filter { case (k,Alias(_)) => false case _ => true }

      sto2 ++ (to flatMap {
        case x@Dyn(s) => subst.get(x).flatMap { 
          case y@Dyn(s2) => 
            eval(y) match {
              case z@Const(_) => 
                Some(s -> z)
              case _ =>
                val z = dealias(y)
                if (z != x)  //HACK
                  Some(s -> Alias(List(z))) else None
            }
          case Static(y) => Some(s -> Const(y))
          case null => Some(s -> null)
        }
        case _ => Nil
      }).toMap

    }
    // TBD: need explicit compare op?
  }

}


