package lancet.interpreter

trait Base_Opt extends Base_Str {

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
    exprs.getOrElse(rhs, {
      if (typeRep[T] == typeRep[Unit]) {
        emit(s.mkString("")); liftConst(()).asInstanceOf[Rep[T]]
      } else {
        val x = fresh; emit("val "+x+" = "+s.mkString("")); Dyn[T](x)
      }
    }).asInstanceOf[Rep[T]]
  }
  def reify[T](x: => Rep[T]): String = ("{\n" + indented(captureOutput(x)) + "\n}")
  // TODO: does reify need to worry about other state like the store?

  var exprs: Map[String, Rep[Any]] = Map.empty

  def rewrite(s: String, x: Rep[Any]): Unit = {
    // not used yet
    assert(false, "REWRITE not used yet")
    exprs += (s -> x)
  }


  abstract class Val[+T]
  case class Const[+T](x: T) extends Val[T] { override def toString = ("Const("+x+")").replace("\n","\\n") }
  case class Partial[+T](fields: Map[String, Rep[Any]]) extends Val[T]
  case class Alias[+T](y: List[Rep[T]]) extends Val[T]
  case object Top extends Val[Nothing]

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
              if (b.toString != str)
                println("val "+str+" = " + b + " // LUBC(" + a + "," + b + ")")
              val tp = bb.typ.asInstanceOf[TypeRep[Any]]
              Dyn[Any](str)(tp)
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
                  val obj = y("alloc")
                  // TODO: make more robust!! can we call runtime.
                  val fld = tp.toString match {
                    case "Int" => 
                      "unsafe.getInt("+obj+","+k+")"
                    case "Object" => 
                      "unsafe.getObject("+obj+","+k+")"
                  }

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
          case (Some(Partial(as)),None) if k.startsWith("CONST") => Partial(lubPartial(k)(as,Map("alloc"->as("alloc"))))
          case (None,Some(Partial(bs))) if k.startsWith("CONST") => Partial(lubPartial(k)(Map("alloc"->bs("alloc")),bs))
          // allocs: may be null in alternative
          //case (Some(Partial(as)),None) => Partial(lubPartial(k)(as,Map("alloc"->liftConst(null))))
          //case (None,Some(Partial(bs))) => Partial(lubPartial(k)(Map("alloc"->liftConst(null)),bs))
          //case (Some(Partial(as)),None) => println("val "+k+" = null // lub "+Partial(as)+", None "); Top
          case (Some(Alias(as)),Some(Alias(bs))) => Alias(as ++ bs)
          case (Some(a),Some(b)) => Top
          case (Some(a),b) => 
            println("// strange lub: "+k+" -> Some("+a+"), "+b); 
            Top //a
          case (a,Some(b)) => 
            println("// strange lub: "+k+" -> "+a+",Some("+b+")"); 
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









trait Core_Opt extends Base_Opt with Core_Str {

  def liftConst[T:TypeRep](x:T): Rep[T] = Static(x)

  // byte/char/short conversion

  override def byteToInt(x: Rep[Byte]): Rep[Int] = eval(x) match {
    case Const(x) => x.toInt
    case _ => super.byteToInt(x)
  }
  override def charToInt(x: Rep[Char]): Rep[Int] = eval(x) match {
    case Const(x) => x.toInt
    case _ => super.charToInt(x)
  }
  override def shortToInt(x: Rep[Short]): Rep[Int] = eval(x) match {
    case Const(x) => x.toInt
    case _ => super.shortToInt(x)
  }


  // int conversion
  override def intToByte(x: Rep[Int]): Rep[Byte] = eval(x) match {
    case Const(x) => x.toByte
    case _ => super.intToByte(x)
  }
  override def intToChar(x: Rep[Int]): Rep[Char] = eval(x) match {
    case Const(x) => x.toChar
    case _ => super.intToChar(x)
  }
  override def intToShort(x: Rep[Int]): Rep[Short] = eval(x) match {
    case Const(x) => x.toShort
    case _ => super.intToShort(x)
  }
  override def intToInt(x: Rep[Int]): Rep[Int] = x
  override def intToLong(x: Rep[Int]): Rep[Long] = eval(x) match {
    case Const(x) => x.toLong
    case _ => super.intToLong(x)
  }
  override def intToFloat(x: Rep[Int]): Rep[Float] = eval(x) match {
    case Const(x) => x.toFloat
    case _ => super.intToFloat(x)
  }
  override def intToDouble(x: Rep[Int]): Rep[Double] = eval(x) match {
    case Const(x) => x.toDouble
    case _ => super.intToDouble(x)
  }

  // int arithmetic
  override def intNegate(x: Rep[Int]): Rep[Int] = eval(x) match {
    case Const(x) => -x
    case _ => super.intNegate(x)
  }
  override def intPlus(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x + y
    case (Const(0), _) => y
    case (_, Const(0)) => x
    case _ => super.intPlus(x,y)
  }
  override def intMinus(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x - y
    case (Const(0), _) => intNegate(y)
    case (_, Const(0)) => x
    case _ => super.intMinus(x,y)
  }
  override def intTimes(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x * y
    case (Const(0), _) => 0
    case (_, Const(0)) => 0
    case (Const(1), _) => y
    case (_, Const(1)) => x
    case (Const(-1), _) => intNegate(y)
    case (_, Const(-1)) => intNegate(x)
    case _ => super.intTimes(x,y)
  }
  override def intDiv(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x / y
    case _ => super.intDiv(x,y)
  }
  override def intMod(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x % y
    case _ => super.intMod(x,y)
  }
  override def intAnd(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x & y
    case _ => super.intAnd(x,y)
  }
  override def intOr(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x | y
    case _ => super.intOr(x,y)
  }
  override def intXor(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x ^ y
    case _ => super.intXor(x,y)
  }
  override def intShiftLeft(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x << y
    case _ => super.intShiftLeft(x,y)
  }
  override def intShiftRight(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x >> y
    case _ => super.intShiftRight(x,y)
  }
  override def intShiftRightUnsigned(x: Rep[Int], y: Rep[Int]): Rep[Int] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x >>> y
    case _ => super.intShiftRightUnsigned(x,y)
  }
  override def intLess(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x < y
    case _ => super.intLess(x,y)
  }
  override def intLessEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x <= y
    case _ => super.intLessEqual(x,y)
  }
  override def intGreater(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x > y
    case _ => super.intGreater(x,y)
  }
  override def intGreaterEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x >= y
    case _ => super.intGreaterEqual(x,y)
  }
  override def intEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x == y
    case _ => super.intEqual(x,y)
  }
  override def intNotEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x != y
    case _ => super.intNotEqual(x,y)
  }

  // long conversion
  override def longToByte(x: Rep[Long]): Rep[Byte] = eval(x) match {
    case Const(x) => x.toByte
    case _ => super.longToByte(x)
  }
  override def longToChar(x: Rep[Long]): Rep[Char] = eval(x) match {
    case Const(x) => x.toChar
    case _ => super.longToChar(x)
  }
  override def longToShort(x: Rep[Long]): Rep[Short] = eval(x) match {
    case Const(x) => x.toShort
    case _ => super.longToShort(x)
  }
  override def longToInt(x: Rep[Long]): Rep[Int] = eval(x) match {
    case Const(x) => x.toInt
    case _ => super.longToInt(x)
  }
  override def longToLong(x: Rep[Long]): Rep[Long] = x
  override def longToFloat(x: Rep[Long]): Rep[Float] = eval(x) match {
    case Const(x) => x.toFloat
    case _ => super.longToFloat(x)
  }
  override def longToDouble(x: Rep[Long]): Rep[Double] = eval(x) match {
    case Const(x) => x.toDouble
    case _ => super.longToDouble(x)
  }

  // long arithmetic
  override def longNegate(x: Rep[Long]): Rep[Long] = eval(x) match {
    case Const(x) => -x
    case _ => super.longNegate(x)
  }
  override def longPlus(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x + y
    case (Const(0), _) => y
    case (_, Const(0)) => x
    case _ => super.longPlus(x,y)
  }
  override def longMinus(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x - y
    case (Const(0), _) => longNegate(y)
    case (_, Const(0)) => x
    case _ => super.longMinus(x,y)
  }
  override def longTimes(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x * y
    case (Const(0), _) => 0
    case (_, Const(0)) => 0
    case (Const(1), _) => y
    case (_, Const(1)) => x
    case (Const(-1), _) => longNegate(y)
    case (_, Const(-1)) => longNegate(x)
    case _ => super.longTimes(x,y)
  }
  override def longDiv(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x / y
    case _ => super.longDiv(x,y)
  }
  override def longMod(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x % y
    case _ => super.longMod(x,y)
  }
  override def longAnd(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x & y
    case _ => super.longAnd(x,y)
  }
  override def longOr(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x | y
    case _ => super.longOr(x,y)
  }
  override def longXor(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x ^ y
    case _ => super.longXor(x,y)
  }
  override def longShiftLeft(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x << y
    case _ => super.longShiftLeft(x,y)
  }
  override def longShiftRight(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x >> y
    case _ => super.longShiftRight(x,y)
  }
  override def longShiftRightUnsigned(x: Rep[Long], y: Rep[Long]): Rep[Long] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x >>> y
    case _ => super.longShiftRightUnsigned(x,y)
  }
  override def longLess(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x < y
    case _ => super.longLess(x,y)
  }
  override def longLessEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x <= y
    case _ => super.longLessEqual(x,y)
  }
  override def longGreater(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x > y
    case _ => super.longGreater(x,y)
  }
  override def longGreaterEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x >= y
    case _ => super.longGreaterEqual(x,y)
  }
  override def longEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x == y
    case _ => super.longEqual(x,y)
  }
  override def longNotEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x != y
    case _ => super.longNotEqual(x,y)
  }

  // float conversion
  override def floatToByte(x: Rep[Float]): Rep[Byte] = eval(x) match {
    case Const(x) => x.toByte
    case _ => super.floatToByte(x)
  }
  override def floatToChar(x: Rep[Float]): Rep[Char] = eval(x) match {
    case Const(x) => x.toChar
    case _ => super.floatToChar(x)
  }
  override def floatToShort(x: Rep[Float]): Rep[Short] = eval(x) match {
    case Const(x) => x.toShort
    case _ => super.floatToShort(x)
  }
  override def floatToInt(x: Rep[Float]): Rep[Int] = eval(x) match {
    case Const(x) => x.toInt
    case _ => super.floatToInt(x)
  }
  override def floatToLong(x: Rep[Float]): Rep[Long] = eval(x) match {
    case Const(x) => x.toLong
    case _ => super.floatToLong(x)
  }
  override def floatToFloat(x: Rep[Float]): Rep[Float] = x
  override def floatToDouble(x: Rep[Float]): Rep[Double] = eval(x) match {
    case Const(x) => x.toDouble
    case _ => super.floatToDouble(x)
  }

  // float arithmetic
  override def floatNegate(x: Rep[Float]): Rep[Float] = eval(x) match {
    case Const(x) => -x
    case _ => super.floatNegate(x)
  }
  override def floatPlus(x: Rep[Float], y: Rep[Float]): Rep[Float] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x + y
    case _ => super.floatPlus(x,y)
  }
  override def floatMinus(x: Rep[Float], y: Rep[Float]): Rep[Float] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x - y
    case _ => super.floatMinus(x,y)
  }
  override def floatTimes(x: Rep[Float], y: Rep[Float]): Rep[Float] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x * y
    case _ => super.floatTimes(x,y)
  }
  override def floatDiv(x: Rep[Float], y: Rep[Float]): Rep[Float] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x / y
    case _ => super.floatDiv(x,y)
  }
  override def floatMod(x: Rep[Float], y: Rep[Float]): Rep[Float] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x % y
    case _ => super.floatMod(x,y)
  }
  override def floatLess(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x < y
    case _ => super.floatLess(x,y)
  }
  override def floatLessEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x <= y
    case _ => super.floatLessEqual(x,y)
  }
  override def floatGreater(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x > y
    case _ => super.floatGreater(x,y)
  }
  override def floatGreaterEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x >= y
    case _ => super.floatGreaterEqual(x,y)
  }
  override def floatEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x == y
    case _ => super.floatEqual(x,y)
  }
  override def floatNotEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x != y
    case _ => super.floatNotEqual(x,y)
  }

  // double conversion
  override def doubleToByte(x: Rep[Double]): Rep[Byte] = eval(x) match {
    case Const(x) => x.toByte
    case _ => super.doubleToByte(x)
  }
  override def doubleToChar(x: Rep[Double]): Rep[Char] = eval(x) match {
    case Const(x) => x.toChar
    case _ => super.doubleToChar(x)
  }
  override def doubleToShort(x: Rep[Double]): Rep[Short] = eval(x) match {
    case Const(x) => x.toShort
    case _ => super.doubleToShort(x)
  }
  override def doubleToInt(x: Rep[Double]): Rep[Int] = eval(x) match {
    case Const(x) => x.toInt
    case _ => super.doubleToInt(x)
  }
  override def doubleToLong(x: Rep[Double]): Rep[Long] = eval(x) match {
    case Const(x) => x.toLong
    case _ => super.doubleToLong(x)
  }
  override def doubleToFloat(x: Rep[Double]): Rep[Float] = eval(x) match {
    case Const(x) => x.toFloat
    case _ => super.doubleToFloat(x)
  }
  override def doubleToDouble(x: Rep[Double]): Rep[Double] = x

  // double arithmetic
  override def doubleNegate(x: Rep[Double]): Rep[Double] = eval(x) match {
    case Const(x) => -x
    case _ => super.doubleNegate(x)
  }
  override def doublePlus(x: Rep[Double], y: Rep[Double]): Rep[Double] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x + y
    case _ => super.doublePlus(x,y)
  }
  override def doubleMinus(x: Rep[Double], y: Rep[Double]): Rep[Double] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x - y
    case _ => super.doubleMinus(x,y)
  }
  override def doubleTimes(x: Rep[Double], y: Rep[Double]): Rep[Double] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x * y
    case _ => super.doubleTimes(x,y)
  }
  override def doubleDiv(x: Rep[Double], y: Rep[Double]): Rep[Double] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x / y
    case _ => super.doubleDiv(x,y)
  }
  override def doubleMod(x: Rep[Double], y: Rep[Double]): Rep[Double] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x % y
    case _ => super.doubleMod(x,y)
  }
  override def doubleLess(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x < y
    case _ => super.doubleLess(x,y)
  }
  override def doubleLessEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x <= y
    case _ => super.doubleLessEqual(x,y)
  }
  override def doubleGreater(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x > y
    case _ => super.doubleGreater(x,y)
  }
  override def doubleGreaterEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x >= y
    case _ => super.doubleGreaterEqual(x,y)
  }
  override def doubleEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x == y
    case _ => super.doubleEqual(x,y)
  }
  override def doubleNotEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x != y
    case _ => super.doubleNotEqual(x,y)
  }


  // object ops
  override def objectEqual(x: Rep[Object], y: Rep[Object]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x eq y
    case (Partial(fs), Const(null)) => false
    case _ => super.objectEqual(x,y)
  }
  override def objectNotEqual(x: Rep[Object], y: Rep[Object]): Rep[Boolean] = (eval(x),eval(y)) match {
    case (Const(x), Const(y)) => x ne y
    case (Partial(fs), Const(null)) => true
    case _ => super.objectNotEqual(x,y)
  }

  override def if_[T:TypeRep](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T] = eval(x) match {
    case Const(x) => if (x) y else z
    case _ => super.if_(x)(y)(z)
  }
}