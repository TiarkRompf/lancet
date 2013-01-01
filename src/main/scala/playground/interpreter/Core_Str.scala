package playground.interpreter


trait Base_Str extends Base {

  def reflect[T:TypeRep](s: Any*): Rep[T]
  def reify[T](x: => Rep[T]): String

  def liftConst[T:TypeRep](x:T): Rep[T]

  def repManifest[T:Manifest]: Manifest[Rep[T]]


  case class TypeRep[T](s: String) { override def toString = s }

  implicit def anyType[T:Manifest] = new TypeRep[T](manifest[T].toString)

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

  def constToString[T](x:T): String = x match {
    case x: Boolean => ""+x
    case x: Int => ""+x
    case x: Long => ""+x
    case x: Double => ""+x
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



  import java.io._
  def captureOutput[A](func: => A): String = {
    val (s,r) = captureOutputResult(func)
    s + r
  }
  def captureOutputResult[A](func: => A): (String,A) = {
    val bstream = new ByteArrayOutputStream
    val r = withOutput(new PrintStream(bstream))(func) //func
    (bstream.toString, r)
  }
  def withOutput[A](out: PrintStream)(func: => A): A = {
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


trait Base_Simple extends Base_Str {

  case class Rep[+T:TypeRep](s: String) { override def toString = s; def typ: TypeRep[_] = implicitly[TypeRep[T]] }
  def repManifest[T:Manifest]: Manifest[Rep[T]] = manifest[Rep[T]]

  var nSyms = 0
  def fresh = { nSyms += 1; "x" + (nSyms - 1) }

  def emit(s: String) = println("          "+s)

  def reflect[T:TypeRep](s: Any*): Rep[T] = { val x = fresh; emit("val "+x+": "+typeRep[T]+" = "+s.mkString("")); Rep[T](x) }
  def reify[T](x: => Rep[T]): String = "{" + captureOutput(x.s) + "}"

  def liftConst[T:TypeRep](x:T): Rep[T] = Rep[T](constToString(x))

}


trait Core_Simple extends Base_Simple with Core_Str


trait Core_Str extends Base_Str {

  implicit def unit(x: Boolean): Rep[Boolean] = liftConst(x)
  implicit def unit(x: Byte): Rep[Byte] = liftConst(x)
  implicit def unit(x: Char): Rep[Char] = liftConst(x)
  implicit def unit(x: Short): Rep[Short] = liftConst(x)
  implicit def unit(x: Int): Rep[Int] = liftConst(x)
  implicit def unit(x: Long): Rep[Long] = liftConst(x)
  implicit def unit(x: Float): Rep[Float] = liftConst(x)
  implicit def unit(x: Double): Rep[Double] = liftConst(x)


  def unit(x: Null): Rep[Object] = liftConst(null)
  def unit(x: Object): Rep[Object] = liftConst(x)


  def byteToInt(x: Rep[Byte]): Rep[Int] = reflect[Int](x,".toInt")
  def charToInt(x: Rep[Char]): Rep[Int] = reflect[Int](x,".toInt")
  def shortToInt(x: Rep[Short]): Rep[Int] = reflect[Int](x,".toInt")


  def intToByte(x: Rep[Int]): Rep[Byte] = reflect[Byte](x,".toByte")
  def intToChar(x: Rep[Int]): Rep[Char] = reflect[Char](x,".toChar")
  def intToShort(x: Rep[Int]): Rep[Short] = reflect[Short](x,".toShort")
  def intToInt(x: Rep[Int]): Rep[Int] = reflect[Int](x,".toInt")
  def intToLong(x: Rep[Int]): Rep[Long] = reflect[Long](x,".toLong")
  def intToFloat(x: Rep[Int]): Rep[Float] = reflect[Float](x,".toFloat")
  def intToDouble(x: Rep[Int]): Rep[Double] = reflect[Double](x,".toDouble")

  def intNegate(x: Rep[Int]): Rep[Int] = reflect[Int]("-",x)
  def intPlus(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," + ",y)
  def intMinus(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," - ",y)
  def intTimes(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," * ",y)
  def intDiv(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," / ",y)
  def intMod(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," % ",y)
  def intAnd(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," & ",y)
  def intOr(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," | ",y)
  def intXor(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," ^ ",y)
  def intShiftLeft(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," << ",y)
  def intShiftRight(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," >> ",y)
  def intShiftRightUnsigned(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](x," >>> ",y)
  def intLess(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](x," < ",y)
  def intLessEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](x," <= ",y)
  def intGreater(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](x," > ",y)
  def intGreaterEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](x," >= ",y)
  def intEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](x," == ",y)
  def intNotEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](x," != ",y)

  def longToByte(x: Rep[Long]): Rep[Byte] = reflect[Byte](x,".toByte")
  def longToChar(x: Rep[Long]): Rep[Char] = reflect[Char](x,".toChar")
  def longToShort(x: Rep[Long]): Rep[Short] = reflect[Short](x,".toShort")
  def longToInt(x: Rep[Long]): Rep[Int] = reflect[Int](x,".toInt")
  def longToLong(x: Rep[Long]): Rep[Long] = reflect[Long](x,".toLong")
  def longToFloat(x: Rep[Long]): Rep[Float] = reflect[Float](x,".toFloat")
  def longToDouble(x: Rep[Long]): Rep[Double] = reflect[Double](x,".toDouble")

  def longNegate(x: Rep[Long]): Rep[Long] = reflect[Long]("-",x)
  def longPlus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," + ",y)
  def longMinus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," - ",y)
  def longTimes(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," * ",y)
  def longDiv(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," / ",y)
  def longMod(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," % ",y)
  def longAnd(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," & ",y)
  def longOr(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," | ",y)
  def longXor(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," ^ ",y)
  def longShiftLeft(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," << ",y)
  def longShiftRight(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," >> ",y)
  def longShiftRightUnsigned(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](x," >>> ",y)
  def longLess(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](x," < ",y)
  def longLessEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](x," <= ",y)
  def longGreater(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](x," > ",y)
  def longGreaterEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](x," >= ",y)
  def longEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](x," == ",y)
  def longNotEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](x," != ",y)

  def floatToByte(x: Rep[Float]): Rep[Byte] = reflect[Byte](x,".toByte")
  def floatToChar(x: Rep[Float]): Rep[Char] = reflect[Char](x,".toChar")
  def floatToShort(x: Rep[Float]): Rep[Short] = reflect[Short](x,".toShort")
  def floatToInt(x: Rep[Float]): Rep[Int] = reflect[Int](x,".toInt")
  def floatToLong(x: Rep[Float]): Rep[Long] = reflect[Long](x,".toLong")
  def floatToFloat(x: Rep[Float]): Rep[Float] = reflect[Float](x,".toFloat")
  def floatToDouble(x: Rep[Float]): Rep[Double] = reflect[Double](x,".toDouble")

  def floatNegate(x: Rep[Float]): Rep[Float] = reflect[Float]("-",x)
  def floatPlus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](x," + ",y)
  def floatMinus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](x," - ",y)
  def floatTimes(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](x," * ",y)
  def floatDiv(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](x," / ",y)
  def floatMod(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](x," % ",y)
  def floatLess(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](x," < ",y)
  def floatLessEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](x," <= ",y)
  def floatGreater(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](x," > ",y)
  def floatGreaterEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](x," >= ",y)
  def floatEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](x," == ",y)
  def floatNotEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](x," != ",y)

  def doubleToByte(x: Rep[Double]): Rep[Byte] = reflect[Byte](x,".toByte")
  def doubleToChar(x: Rep[Double]): Rep[Char] = reflect[Char](x,".toChar")
  def doubleToShort(x: Rep[Double]): Rep[Short] = reflect[Short](x,".toShort")
  def doubleToInt(x: Rep[Double]): Rep[Int] = reflect[Int](x,".toInt")
  def doubleToLong(x: Rep[Double]): Rep[Long] = reflect[Long](x,".toLong")
  def doubleToFloat(x: Rep[Double]): Rep[Float] = reflect[Float](x,".toFloat")
  def doubleToDouble(x: Rep[Double]): Rep[Double] = reflect[Double](x,".toDouble")

  def doubleNegate(x: Rep[Double]): Rep[Double] = reflect[Double]("-",x)
  def doublePlus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](x," + ",y)
  def doubleMinus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](x," - ",y)
  def doubleTimes(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](x," * ",y)
  def doubleDiv(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](x," / ",y)
  def doubleMod(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](x," % ",y)
  def doubleLess(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](x," < ",y)
  def doubleLessEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](x," <= ",y)
  def doubleGreater(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](x," > ",y)
  def doubleGreaterEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](x," >= ",y)
  def doubleEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](x," == ",y)
  def doubleNotEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](x," != ",y)

  def objectEqual(x: Rep[Object], y: Rep[Object]): Rep[Boolean] = reflect[Boolean](x," eq ",y)
  def objectNotEqual(x: Rep[Object], y: Rep[Object]): Rep[Boolean] = reflect[Boolean](x," ne ",y)
  def objectAsInstanceOf[T:TypeRep](x: Rep[Object]): Rep[T] = reflect[T](x,".asInstanceOf[",typeRep[T],"]")

  def if_[T:TypeRep](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T] =
    reflect[T]("if (",x,") ",reify(y)," else ",reify(z))

}

