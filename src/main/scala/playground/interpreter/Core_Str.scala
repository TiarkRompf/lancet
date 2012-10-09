package playground.interpreter


trait Base_Str extends Base {

  case class Rep[+T](s: String) { override def toString = s; def +(s: String) = toString+s }
  def repManifest[T:Manifest]: Manifest[Rep[T]] = manifest[Rep[T]]

  var nSyms = 0
  def fresh = { nSyms += 1; "x" + (nSyms - 1) }

  def emit(s: String) = println("          "+s)

  def reflect[T](s: Any*): Rep[T] = { val x = fresh; emit("val "+x+" = "+s.mkString("")); Rep(x) }
  def reify[T](x: => Rep[T]): String = "{" + captureOutput(x.s) + "}"

  import java.io._
  def captureOutput(func: => Any): String = {
    val bstream = new ByteArrayOutputStream
    val r = withOutput(new PrintStream(bstream))(func)
    bstream.toString + r
  }
  def withOutput(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      scala.Console.withOut(out)(scala.Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }

}

trait Core_Str extends Base_Str with Core_StrCommon {
  def liftConst[T](x:T): Rep[T] = Rep(""+x)
}

trait Core_StrCommon extends Core {
  def liftConst[T](x:T): Rep[T]
  def reflect[T](s: Any*): Rep[T]
  def reify[T](x: => Rep[T]): String

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


  def byteToInt(x: Rep[Byte]): Rep[Int] = reflect(x,".toInt")
  def charToInt(x: Rep[Char]): Rep[Int] = reflect(x,".toInt")
  def shortToInt(x: Rep[Short]): Rep[Int] = reflect(x,".toInt")


  def intToByte(x: Rep[Int]): Rep[Byte] = reflect(x,".toByte")
  def intToChar(x: Rep[Int]): Rep[Char] = reflect(x,".toChar")
  def intToShort(x: Rep[Int]): Rep[Short] = reflect(x,".toShort")
  def intToInt(x: Rep[Int]): Rep[Int] = reflect(x,".toInt")
  def intToLong(x: Rep[Int]): Rep[Long] = reflect(x,".toLong")
  def intToFloat(x: Rep[Int]): Rep[Float] = reflect(x,".toFloat")
  def intToDouble(x: Rep[Int]): Rep[Double] = reflect(x,".toDouble")

  def intNegate(x: Rep[Int]): Rep[Int] = reflect("-",x)
  def intPlus(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," + ",y)
  def intMinus(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," - ",y)
  def intTimes(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," * ",y)
  def intDiv(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," / ",y)
  def intMod(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," % ",y)
  def intAnd(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," & ",y)
  def intOr(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," | ",y)
  def intXor(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," ^ ",y)
  def intShiftLeft(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," << ",y)
  def intShiftRight(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," >> ",y)
  def intShiftRightUnsigned(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x," >>> ",y)
  def intLess(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x," < ",y)
  def intLessEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x," <= ",y)
  def intGreater(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x," > ",y)
  def intGreaterEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x," >= ",y)
  def intEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x," == ",y)
  def intNotEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x," != ",y)

  def longToByte(x: Rep[Long]): Rep[Byte] = reflect(x,".toByte")
  def longToChar(x: Rep[Long]): Rep[Char] = reflect(x,".toChar")
  def longToShort(x: Rep[Long]): Rep[Short] = reflect(x,".toShort")
  def longToInt(x: Rep[Long]): Rep[Int] = reflect(x,".toInt")
  def longToLong(x: Rep[Long]): Rep[Long] = reflect(x,".toLong")
  def longToFloat(x: Rep[Long]): Rep[Float] = reflect(x,".toFloat")
  def longToDouble(x: Rep[Long]): Rep[Double] = reflect(x,".toDouble")

  def longNegate(x: Rep[Long]): Rep[Long] = reflect("-",x)
  def longPlus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," + ",y)
  def longMinus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," - ",y)
  def longTimes(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," * ",y)
  def longDiv(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," / ",y)
  def longMod(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," % ",y)
  def longAnd(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," & ",y)
  def longOr(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," | ",y)
  def longXor(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," ^ ",y)
  def longShiftLeft(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," << ",y)
  def longShiftRight(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," >> ",y)
  def longShiftRightUnsigned(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x," >>> ",y)
  def longLess(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x," < ",y)
  def longLessEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x," <= ",y)
  def longGreater(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x," > ",y)
  def longGreaterEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x," >= ",y)
  def longEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x," == ",y)
  def longNotEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x," != ",y)

  def floatToByte(x: Rep[Float]): Rep[Byte] = reflect(x,".toByte")
  def floatToChar(x: Rep[Float]): Rep[Char] = reflect(x,".toChar")
  def floatToShort(x: Rep[Float]): Rep[Short] = reflect(x,".toShort")
  def floatToInt(x: Rep[Float]): Rep[Int] = reflect(x,".toInt")
  def floatToLong(x: Rep[Float]): Rep[Long] = reflect(x,".toLong")
  def floatToFloat(x: Rep[Float]): Rep[Float] = reflect(x,".toFloat")
  def floatToDouble(x: Rep[Float]): Rep[Double] = reflect(x,".toDouble")

  def floatNegate(x: Rep[Float]): Rep[Float] = reflect("-",x)
  def floatPlus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x," + ",y)
  def floatMinus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x," - ",y)
  def floatTimes(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x," * ",y)
  def floatDiv(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x," / ",y)
  def floatMod(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x," % ",y)
  def floatLess(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x," < ",y)
  def floatLessEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x," <= ",y)
  def floatGreater(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x," > ",y)
  def floatGreaterEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x," >= ",y)
  def floatEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x," == ",y)
  def floatNotEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x," != ",y)

  def doubleToByte(x: Rep[Double]): Rep[Byte] = reflect(x,".toByte")
  def doubleToChar(x: Rep[Double]): Rep[Char] = reflect(x,".toChar")
  def doubleToShort(x: Rep[Double]): Rep[Short] = reflect(x,".toShort")
  def doubleToInt(x: Rep[Double]): Rep[Int] = reflect(x,".toInt")
  def doubleToLong(x: Rep[Double]): Rep[Long] = reflect(x,".toLong")
  def doubleToFloat(x: Rep[Double]): Rep[Float] = reflect(x,".toFloat")
  def doubleToDouble(x: Rep[Double]): Rep[Double] = reflect(x,".toDouble")

  def doubleNegate(x: Rep[Double]): Rep[Double] = reflect("-",x)
  def doublePlus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x," + ",y)
  def doubleMinus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x," - ",y)
  def doubleTimes(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x," * ",y)
  def doubleDiv(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x," / ",y)
  def doubleMod(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x," % ",y)
  def doubleLess(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x," < ",y)
  def doubleLessEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x," <= ",y)
  def doubleGreater(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x," > ",y)
  def doubleGreaterEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x," >= ",y)
  def doubleEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x," == ",y)
  def doubleNotEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x," != ",y)


  def if_[T](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T] =
    reflect("if (",x,") ",reify(y)," else ",reify(z))

}


trait Base_Opt extends Base {

  abstract class Rep[+T] { def +(s: String) = toString+s }

  case class Const[+T](x: T) extends Rep[T] { override def toString = "" + x }
  case class Dyn[+T](s: String) extends Rep[T] { override def toString = s }

  def repManifest[T:Manifest]: Manifest[Rep[T]] = manifest[Rep[T]]

  var nSyms = 0
  def fresh = { nSyms += 1; "x" + (nSyms - 1) }

  def emit(s: String) = println("          "+s)

  def reflect[T](s: Any*): Rep[T] = { val x = fresh; emit("val "+x+" = "+s.mkString("")); Dyn(x) }
  def reify[T](x: => Rep[T]): String = "{" + captureOutput(x) + "}"

  import java.io._
  def captureOutput(func: => Any): String = {
    val bstream = new ByteArrayOutputStream
    val r = withOutput(new PrintStream(bstream))(func)
    bstream.toString + r
  }
  def withOutput(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      scala.Console.withOut(out)(scala.Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }

}




trait Core_Opt extends Base_Opt with Core_StrCommon {

  def liftConst[T](x:T): Rep[T] = Const(x)


  override def byteToInt(x: Rep[Byte]): Rep[Int] = x match {
    case Const(x) => x.toInt
    case _ => super.byteToInt(x)
  }
  override def charToInt(x: Rep[Char]): Rep[Int] = x match {
    case Const(x) => x.toInt
    case _ => super.charToInt(x)
  }
  override def shortToInt(x: Rep[Short]): Rep[Int] = x match {
    case Const(x) => x.toInt
    case _ => super.shortToInt(x)
  }

  override def intToByte(x: Rep[Int]): Rep[Byte] = x match {
    case Const(x) => x.toByte
    case _ => super.intToByte(x)
  }
  override def intToChar(x: Rep[Int]): Rep[Char] = x match {
    case Const(x) => x.toChar
    case _ => super.intToChar(x)
  }
  override def intToShort(x: Rep[Int]): Rep[Short] = x match {
    case Const(x) => x.toShort
    case _ => super.intToShort(x)
  }
  override def intToInt(x: Rep[Int]): Rep[Int] = x
  override def intToLong(x: Rep[Int]): Rep[Long] = x match {
    case Const(x) => x.toLong
    case _ => super.intToLong(x)
  }
  override def intToFloat(x: Rep[Int]): Rep[Float] = x match {
    case Const(x) => x.toFloat
    case _ => super.intToFloat(x)
  }
  override def intToDouble(x: Rep[Int]): Rep[Double] = x match {
    case Const(x) => x.toDouble
    case _ => super.intToDouble(x)
  }

  override def intNegate(x: Rep[Int]): Rep[Int] = x match {
    case Const(x) => -x
    case _ => super.intNegate(x)
  }
  override def intPlus(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x + y
    case (Const(0), _) => y
    case (_, Const(0)) => x
    case _ => super.intPlus(x,y)
  }
  override def intMinus(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x - y
    case (Const(0), _) => intNegate(y)
    case (_, Const(0)) => x
    case _ => super.intMinus(x,y)
  }
  override def intTimes(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x * y
    case (Const(0), _) => 0
    case (_, Const(0)) => 0
    case (Const(1), _) => y
    case (_, Const(1)) => x
    case (Const(-1), _) => intNegate(y)
    case (_, Const(-1)) => intNegate(x)
    case _ => super.intTimes(x,y)
  }
  override def intDiv(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x / y
    case _ => super.intDiv(x,y)
  }
  override def intMod(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x % y
    case _ => super.intMod(x,y)
  }
  override def intAnd(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x & y
    case _ => super.intAnd(x,y)
  }
  override def intOr(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x | y
    case _ => super.intOr(x,y)
  }
  override def intXor(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x ^ y
    case _ => super.intXor(x,y)
  }
  override def intShiftLeft(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x << y
    case _ => super.intShiftLeft(x,y)
  }
  override def intShiftRight(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x >> y
    case _ => super.intShiftRight(x,y)
  }
  override def intShiftRightUnsigned(x: Rep[Int], y: Rep[Int]): Rep[Int] = (x,y) match {
    case (Const(x), Const(y)) => x >>> y
    case _ => super.intShiftRightUnsigned(x,y)
  }
  override def intLess(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (x,y) match {
    case (Const(x), Const(y)) => x < y
    case _ => super.intLess(x,y)
  }
  override def intLessEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (x,y) match {
    case (Const(x), Const(y)) => x <= y
    case _ => super.intLessEqual(x,y)
  }
  override def intGreater(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (x,y) match {
    case (Const(x), Const(y)) => x > y
    case _ => super.intGreater(x,y)
  }
  override def intGreaterEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (x,y) match {
    case (Const(x), Const(y)) => x >= y
    case _ => super.intGreaterEqual(x,y)
  }
  override def intEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (x,y) match {
    case (Const(x), Const(y)) => x == y
    case _ => super.intEqual(x,y)
  }
  override def intNotEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = (x,y) match {
    case (Const(x), Const(y)) => x != y
    case _ => super.intNotEqual(x,y)
  }

  override def longToByte(x: Rep[Long]): Rep[Byte] = reflect(x+".toByte")
  override def longToChar(x: Rep[Long]): Rep[Char] = reflect(x+".toChar")
  override def longToShort(x: Rep[Long]): Rep[Short] = reflect(x+".toShort")
  override def longToInt(x: Rep[Long]): Rep[Int] = reflect(x+".toInt")
  override def longToLong(x: Rep[Long]): Rep[Long] = reflect(x+".toLong")
  override def longToFloat(x: Rep[Long]): Rep[Float] = reflect(x+".toFloat")
  override def longToDouble(x: Rep[Long]): Rep[Double] = reflect(x+".toDouble")

  override def longNegate(x: Rep[Long]): Rep[Long] = reflect("-"+x)
  override def longPlus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" + "+y)
  override def longMinus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" - "+y)
  override def longTimes(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" * "+y)
  override def longDiv(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" / "+y)
  override def longMod(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" % "+y)
  override def longAnd(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" & "+y)
  override def longOr(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" | "+y)
  override def longXor(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" ^ "+y)
  override def longShiftLeft(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" << "+y)
  override def longShiftRight(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" >> "+y)
  override def longShiftRightUnsigned(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" >>> "+y)
  override def longLess(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" < "+y)
  override def longLessEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" <= "+y)
  override def longGreater(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" > "+y)
  override def longGreaterEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" >= "+y)
  override def longEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" == "+y)
  override def longNotEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" != "+y)

  override def floatToByte(x: Rep[Float]): Rep[Byte] = reflect(x+".toByte")
  override def floatToChar(x: Rep[Float]): Rep[Char] = reflect(x+".toChar")
  override def floatToShort(x: Rep[Float]): Rep[Short] = reflect(x+".toShort")
  override def floatToInt(x: Rep[Float]): Rep[Int] = reflect(x+".toInt")
  override def floatToLong(x: Rep[Float]): Rep[Long] = reflect(x+".toLong")
  override def floatToFloat(x: Rep[Float]): Rep[Float] = reflect(x+".toFloat")
  override def floatToDouble(x: Rep[Float]): Rep[Double] = reflect(x+".toDouble")

  override def floatNegate(x: Rep[Float]): Rep[Float] = reflect("-"+x)
  override def floatPlus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" + "+y)
  override def floatMinus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" - "+y)
  override def floatTimes(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" * "+y)
  override def floatDiv(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" / "+y)
  override def floatMod(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" % "+y)
  override def floatLess(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" < "+y)
  override def floatLessEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" <= "+y)
  override def floatGreater(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" > "+y)
  override def floatGreaterEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" >= "+y)
  override def floatEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" == "+y)
  override def floatNotEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" != "+y)

  override def doubleToByte(x: Rep[Double]): Rep[Byte] = reflect(x+".toByte")
  override def doubleToChar(x: Rep[Double]): Rep[Char] = reflect(x+".toChar")
  override def doubleToShort(x: Rep[Double]): Rep[Short] = reflect(x+".toShort")
  override def doubleToInt(x: Rep[Double]): Rep[Int] = reflect(x+".toInt")
  override def doubleToLong(x: Rep[Double]): Rep[Long] = reflect(x+".toLong")
  override def doubleToFloat(x: Rep[Double]): Rep[Float] = reflect(x+".toFloat")
  override def doubleToDouble(x: Rep[Double]): Rep[Double] = reflect(x+".toDouble")

  override def doubleNegate(x: Rep[Double]): Rep[Double] = reflect("-"+x)
  override def doublePlus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" + "+y)
  override def doubleMinus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" - "+y)
  override def doubleTimes(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" * "+y)
  override def doubleDiv(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" / "+y)
  override def doubleMod(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" % "+y)
  override def doubleLess(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" < "+y)
  override def doubleLessEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" <= "+y)
  override def doubleGreater(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" > "+y)
  override def doubleGreaterEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" >= "+y)
  override def doubleEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" == "+y)
  override def doubleNotEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" != "+y)


  override def if_[T](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T] =
    reflect("if ("+x+") "+reify(y)+" else "+reify(z))

}

