package playground.interpreter

// TODO: use Scala-Virtualized

trait Base {
  type Rep[+T]

  /*implicit def unitWiden[T <% Boolean](x: T): Rep[Boolean]
  implicit def unitWiden[T <% Byte](x: T): Rep[Byte]
  implicit def unitWiden[T <% Char](x: T): Rep[Char]
  implicit def unitWiden[T <% Short](x: T): Rep[Short]
  implicit def unitWiden[T <% Int](x: T): Rep[Int]
  implicit def unitWiden[T <% Long](x: T): Rep[Long]
  implicit def unitWiden[T <% Float](x: T): Rep[Float]
  implicit def unitWiden[T <% Double](x: T): Rep[Double]*/

  implicit def unit(x: Boolean): Rep[Boolean]
  implicit def unit(x: Byte): Rep[Byte]
  implicit def unit(x: Char): Rep[Char]
  implicit def unit(x: Short): Rep[Short]
  implicit def unit(x: Int): Rep[Int]
  implicit def unit(x: Long): Rep[Long]
  implicit def unit(x: Float): Rep[Float]
  implicit def unit(x: Double): Rep[Double]

  //implicit def unitConv[T,U](x: T)(implicit lift: T => U, conv: U => Rep[U]): Rep[U] = conv(lift(x))


  implicit def unit(x: Null): Rep[Object]
  def unit(x: Object): Rep[Object]


  // incomplete
  implicit def byte2int(x: Rep[Byte]): Rep[Int] = byteToInt(x)
  implicit def char2int(x: Rep[Char]): Rep[Int] = charToInt(x)
  implicit def short2int(x: Rep[Short]): Rep[Int] = shortToInt(x)
  implicit def int2long(x: Rep[Int]): Rep[Long] = intToLong(x)
  implicit def int2float(x: Rep[Int]): Rep[Float] = intToFloat(x)
  implicit def int2double(x: Rep[Int]): Rep[Double] = intToDouble(x)
  implicit def long2float(x: Rep[Long]): Rep[Float] = longToFloat(x)
  implicit def long2double(x: Rep[Long]): Rep[Double] = longToDouble(x)
  implicit def float2double(x: Rep[Float]): Rep[Double] = floatToDouble(x)

  def byteToInt(x: Rep[Byte]): Rep[Int]
  def charToInt(x: Rep[Char]): Rep[Int]
  def shortToInt(x: Rep[Short]): Rep[Int]

  implicit def intOps(x: Rep[Int]) = new IntOps(x)
  class IntOps(x: Rep[Int]) {
    def toByte: Rep[Byte] = intToByte(x)
    def toChar: Rep[Char] = intToChar(x)
    def toShort: Rep[Short] = intToShort(x)
    def toInt: Rep[Int] = intToInt(x)
    def toLong: Rep[Long] = intToLong(x)
    def toFloat: Rep[Float] = intToFloat(x)
    def toDouble: Rep[Double] = intToDouble(x)
    def unary_- : Rep[Int] = intNegate(x)
    def +(y: Rep[Int]): Rep[Int] = intPlus(x,y)
    def -(y: Rep[Int]): Rep[Int] = intMinus(x,y)
    def *(y: Rep[Int]): Rep[Int] = intTimes(x,y)
    def /(y: Rep[Int]): Rep[Int] = intDiv(x,y)
    def %(y: Rep[Int]): Rep[Int] = intMod(x,y)
    def &(y: Rep[Int]): Rep[Int] = intAnd(x,y)
    def |(y: Rep[Int]): Rep[Int] = intOr(x,y)
    def ^(y: Rep[Int]): Rep[Int] = intXor(x,y)
    def <<(y: Rep[Int]): Rep[Int] = intShiftLeft(x,y)
    def >>(y: Rep[Int]): Rep[Int] = intShiftRight(x,y)
    def >>>(y: Rep[Int]): Rep[Int] = intShiftRightUnsigned(x,y)
    def <(y: Rep[Int]): Rep[Boolean] = intLess(x,y)
    def <=(y: Rep[Int]): Rep[Boolean] = intLessEqual(x,y)
    def >(y: Rep[Int]): Rep[Boolean] = intGreater(x,y)
    def >=(y: Rep[Int]): Rep[Boolean] = intGreaterEqual(x,y)
    def ===(y: Rep[Int]): Rep[Boolean] = intEqual(x,y)
    def !==(y: Rep[Int]): Rep[Boolean] = intNotEqual(x,y)
  }

  def intToByte(x: Rep[Int]): Rep[Byte]
  def intToChar(x: Rep[Int]): Rep[Char]
  def intToShort(x: Rep[Int]): Rep[Short]
  def intToInt(x: Rep[Int]): Rep[Int]
  def intToLong(x: Rep[Int]): Rep[Long]
  def intToFloat(x: Rep[Int]): Rep[Float]
  def intToDouble(x: Rep[Int]): Rep[Double]

  def intNegate(x: Rep[Int]): Rep[Int]
  def intPlus(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intMinus(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intTimes(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intDiv(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intMod(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intAnd(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intOr(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intXor(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intShiftLeft(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intShiftRight(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intShiftRightUnsigned(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def intLess(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intLessEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intGreater(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intGreaterEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def intNotEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean]



  implicit def int2longOps(x: Int): LongOps = new LongOps(unit(x))
  implicit def long2longOps(x: Long): LongOps = new LongOps(unit(x))
  implicit def longOps(x: Rep[Long]) = new LongOps(x)
  class LongOps(x: Rep[Long]) {
    def toByte: Rep[Byte] = longToByte(x)
    def toChar: Rep[Char] = longToChar(x)
    def toShort: Rep[Short] = longToShort(x)
    def toInt: Rep[Int] = longToInt(x)
    def toLong: Rep[Long] = longToLong(x)
    def toFloat: Rep[Float] = longToFloat(x)
    def toDouble: Rep[Double] = longToDouble(x)
    def unary_- : Rep[Long] = longNegate(x)
    def +(y: Rep[Long]): Rep[Long] = longPlus(x,y)
    def -(y: Rep[Long]): Rep[Long] = longMinus(x,y)
    def *(y: Rep[Long]): Rep[Long] = longTimes(x,y)
    def /(y: Rep[Long]): Rep[Long] = longDiv(x,y)
    def %(y: Rep[Long]): Rep[Long] = longMod(x,y)
    def &(y: Rep[Long]): Rep[Long] = longAnd(x,y)
    def |(y: Rep[Long]): Rep[Long] = longOr(x,y)
    def ^(y: Rep[Long]): Rep[Long] = longXor(x,y)
    def <<(y: Rep[Long]): Rep[Long] = longShiftLeft(x,y)
    def >>(y: Rep[Long]): Rep[Long] = longShiftRight(x,y)
    def >>>(y: Rep[Long]): Rep[Long] = longShiftRightUnsigned(x,y)
    def <(y: Rep[Long]): Rep[Boolean] = longLess(x,y)
    def <=(y: Rep[Long]): Rep[Boolean] = longLessEqual(x,y)
    def >(y: Rep[Long]): Rep[Boolean] = longGreater(x,y)
    def >=(y: Rep[Long]): Rep[Boolean] = longGreaterEqual(x,y)
    def ===(y: Rep[Long]): Rep[Boolean] = longEqual(x,y)
    def !==(y: Rep[Long]): Rep[Boolean] = longNotEqual(x,y)
  }

  def longToByte(x: Rep[Long]): Rep[Byte]
  def longToChar(x: Rep[Long]): Rep[Char]
  def longToShort(x: Rep[Long]): Rep[Short]
  def longToInt(x: Rep[Long]): Rep[Int]
  def longToLong(x: Rep[Long]): Rep[Long]
  def longToFloat(x: Rep[Long]): Rep[Float]
  def longToDouble(x: Rep[Long]): Rep[Double]

  def longNegate(x: Rep[Long]): Rep[Long]
  def longPlus(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longMinus(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longTimes(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longDiv(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longMod(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longAnd(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longOr(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longXor(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longShiftLeft(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longShiftRight(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longShiftRightUnsigned(x: Rep[Long], y: Rep[Long]): Rep[Long]
  def longLess(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longLessEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longGreater(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longGreaterEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean]
  def longNotEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean]

  implicit def floatOps(x: Rep[Float]) = new FloatOps(x)
  class FloatOps(x: Rep[Float]) {
    def toByte: Rep[Byte] = floatToByte(x)
    def toChar: Rep[Char] = floatToChar(x)
    def toShort: Rep[Short] = floatToShort(x)
    def toInt: Rep[Int] = floatToInt(x)
    def toLong: Rep[Long] = floatToLong(x)
    def toFloat: Rep[Float] = floatToFloat(x)
    def toDouble: Rep[Double] = floatToDouble(x)
    def unary_- : Rep[Float] = floatNegate(x)
    def +(y: Rep[Float]): Rep[Float] = floatPlus(x,y)
    def -(y: Rep[Float]): Rep[Float] = floatMinus(x,y)
    def *(y: Rep[Float]): Rep[Float] = floatTimes(x,y)
    def /(y: Rep[Float]): Rep[Float] = floatDiv(x,y)
    def %(y: Rep[Float]): Rep[Float] = floatMod(x,y)
    def <(y: Rep[Float]): Rep[Boolean] = floatLess(x,y)
    def <=(y: Rep[Float]): Rep[Boolean] = floatLessEqual(x,y)
    def >(y: Rep[Float]): Rep[Boolean] = floatGreater(x,y)
    def >=(y: Rep[Float]): Rep[Boolean] = floatGreaterEqual(x,y)
    def ===(y: Rep[Float]): Rep[Boolean] = floatEqual(x,y)
    def =!=(y: Rep[Float]): Rep[Boolean] = floatNotEqual(x,y)
  }

  def floatToByte(x: Rep[Float]): Rep[Byte]
  def floatToChar(x: Rep[Float]): Rep[Char]
  def floatToShort(x: Rep[Float]): Rep[Short]
  def floatToInt(x: Rep[Float]): Rep[Int]
  def floatToLong(x: Rep[Float]): Rep[Long]
  def floatToFloat(x: Rep[Float]): Rep[Float]
  def floatToDouble(x: Rep[Float]): Rep[Double]

  def floatNegate(x: Rep[Float]): Rep[Float]
  def floatPlus(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatMinus(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatTimes(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatDiv(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatMod(x: Rep[Float], y: Rep[Float]): Rep[Float]
  def floatLess(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatLessEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatGreater(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatGreaterEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean]
  def floatNotEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean]

  implicit def doubleOps(x: Rep[Double]) = new DoubleOps(x)
  class DoubleOps(x: Rep[Double]) {
    def toByte: Rep[Byte] = doubleToByte(x)
    def toChar: Rep[Char] = doubleToChar(x)
    def toShort: Rep[Short] = doubleToShort(x)
    def toInt: Rep[Int] = doubleToInt(x)
    def toLong: Rep[Long] = doubleToLong(x)
    def toFloat: Rep[Float] = doubleToFloat(x)
    def toDouble: Rep[Double] = doubleToDouble(x)
    def unary_- : Rep[Double] = doubleNegate(x)
    def +(y: Rep[Double]): Rep[Double] = doublePlus(x,y)
    def -(y: Rep[Double]): Rep[Double] = doubleMinus(x,y)
    def *(y: Rep[Double]): Rep[Double] = doubleTimes(x,y)
    def /(y: Rep[Double]): Rep[Double] = doubleDiv(x,y)
    def %(y: Rep[Double]): Rep[Double] = doubleMod(x,y)
    def <(y: Rep[Double]): Rep[Boolean] = doubleLess(x,y)
    def <=(y: Rep[Double]): Rep[Boolean] = doubleLessEqual(x,y)
    def >(y: Rep[Double]): Rep[Boolean] = doubleGreater(x,y)
    def >=(y: Rep[Double]): Rep[Boolean] = doubleGreaterEqual(x,y)
    def ===(y: Rep[Double]): Rep[Boolean] = doubleEqual(x,y)
    def !==(y: Rep[Double]): Rep[Boolean] = doubleNotEqual(x,y)
  }

  def doubleToByte(x: Rep[Double]): Rep[Byte]
  def doubleToChar(x: Rep[Double]): Rep[Char]
  def doubleToShort(x: Rep[Double]): Rep[Short]
  def doubleToInt(x: Rep[Double]): Rep[Int]
  def doubleToLong(x: Rep[Double]): Rep[Long]
  def doubleToFloat(x: Rep[Double]): Rep[Float]
  def doubleToDouble(x: Rep[Double]): Rep[Double]

  def doubleNegate(x: Rep[Double]): Rep[Double]
  def doublePlus(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleMinus(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleTimes(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleDiv(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleMod(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def doubleLess(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleLessEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleGreater(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleGreaterEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean]
  def doubleNotEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean]


  def if_[T](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T]

}

trait Base_Impl extends Base {
  type Rep[+T] = T

  implicit def unit(x: Boolean): Boolean = x
  implicit def unit(x: Byte): Byte = x
  implicit def unit(x: Char): Char = x
  implicit def unit(x: Short): Short = x
  implicit def unit(x: Int): Int = x
  implicit def unit(x: Long): Long = x
  implicit def unit(x: Float): Float = x
  implicit def unit(x: Double): Double = x

  def unit(x: Null): Object = x
  def unit(x: Object): Rep[Object] = x

  def byteToInt(x: Byte): Int = x.toInt
  def charToInt(x: Char): Int = x.toInt
  def shortToInt(x: Short): Int = x.toInt

  def intToByte(x: Int): Byte = x.toByte
  def intToChar(x: Int): Char = x.toChar
  def intToShort(x: Int): Short = x.toShort
  def intToInt(x: Int): Int = x.toInt
  def intToLong(x: Int): Long = x.toLong
  def intToFloat(x: Int): Float = x.toFloat
  def intToDouble(x: Int): Double = x.toDouble

  def intNegate(x: Int): Int = -x
  def intPlus(x: Int, y: Int): Int = x + y
  def intMinus(x: Int, y: Int): Int = x - y
  def intTimes(x: Int, y: Int): Int = x * y
  def intDiv(x: Int, y: Int): Int = x / y
  def intMod(x: Int, y: Int): Int = x % y
  def intAnd(x: Int, y: Int): Int = x & y
  def intOr(x: Int, y: Int): Int = x | y
  def intXor(x: Int, y: Int): Int = x ^ y
  def intShiftLeft(x: Int, y: Int): Int = x << y
  def intShiftRight(x: Int, y: Int): Int = x >> y
  def intShiftRightUnsigned(x: Int, y: Int): Int = x >>> y
  def intLess(x: Int, y: Int): Boolean = x < y
  def intLessEqual(x: Int, y: Int): Boolean = x <= y
  def intGreater(x: Int, y: Int): Boolean = x > y
  def intGreaterEqual(x: Int, y: Int): Boolean = x >= y
  def intEqual(x: Int, y: Int): Boolean = x == y
  def intNotEqual(x: Int, y: Int): Boolean = x != y

  def longToByte(x: Long): Byte = x.toByte
  def longToChar(x: Long): Char = x.toChar
  def longToShort(x: Long): Short = x.toShort
  def longToInt(x: Long): Int = x.toInt
  def longToLong(x: Long): Long = x.toLong
  def longToFloat(x: Long): Float = x.toFloat
  def longToDouble(x: Long): Double = x.toDouble

  def longNegate(x: Long): Long = -x
  def longPlus(x: Long, y: Long): Long = x + y
  def longMinus(x: Long, y: Long): Long = x - y
  def longTimes(x: Long, y: Long): Long = x * y
  def longDiv(x: Long, y: Long): Long = x / y
  def longMod(x: Long, y: Long): Long = x % y
  def longAnd(x: Long, y: Long): Long = x & y
  def longOr(x: Long, y: Long): Long = x | y
  def longXor(x: Long, y: Long): Long = x ^ y
  def longShiftLeft(x: Long, y: Long): Long = x << y
  def longShiftRight(x: Long, y: Long): Long = x >> y
  def longShiftRightUnsigned(x: Long, y: Long): Long = x >>> y
  def longLess(x: Long, y: Long): Boolean = x < y
  def longLessEqual(x: Long, y: Long): Boolean = x <= y
  def longGreater(x: Long, y: Long): Boolean = x > y
  def longGreaterEqual(x: Long, y: Long): Boolean = x >= y
  def longEqual(x: Long, y: Long): Boolean = x == y
  def longNotEqual(x: Long, y: Long): Boolean = x != y

  
  def floatToByte(x: Float): Byte = x.toByte
  def floatToChar(x: Float): Char = x.toChar
  def floatToShort(x: Float): Short = x.toShort
  def floatToInt(x: Float): Int = x.toInt
  def floatToLong(x: Float): Long = x.toLong
  def floatToFloat(x: Float): Float = x.toFloat
  def floatToDouble(x: Float): Double = x.toDouble

  def floatNegate(x: Float): Float = -x
  def floatPlus(x: Float, y: Float): Float = x + y
  def floatMinus(x: Float, y: Float): Float = x - y
  def floatTimes(x: Float, y: Float): Float = x * y
  def floatDiv(x: Float, y: Float): Float = x / y
  def floatMod(x: Float, y: Float): Float = x % y
  def floatLess(x: Float, y: Float): Boolean = x < y
  def floatLessEqual(x: Float, y: Float): Boolean = x <= y
  def floatGreater(x: Float, y: Float): Boolean = x > y
  def floatGreaterEqual(x: Float, y: Float): Boolean = x >= y
  def floatEqual(x: Float, y: Float): Boolean = x == y
  def floatNotEqual(x: Float, y: Float): Boolean = x != y

  def doubleToByte(x: Double): Byte = x.toByte
  def doubleToChar(x: Double): Char = x.toChar
  def doubleToShort(x: Double): Short = x.toShort
  def doubleToInt(x: Double): Int = x.toInt
  def doubleToLong(x: Double): Long = x.toLong
  def doubleToFloat(x: Double): Float = x.toFloat
  def doubleToDouble(x: Double): Double = x.toDouble

  def doubleNegate(x: Double): Double = -x
  def doublePlus(x: Double, y: Double): Double = x + y
  def doubleMinus(x: Double, y: Double): Double = x - y
  def doubleTimes(x: Double, y: Double): Double = x * y
  def doubleDiv(x: Double, y: Double): Double = x / y
  def doubleMod(x: Double, y: Double): Double = x % y
  def doubleLess(x: Double, y: Double): Boolean = x < y
  def doubleLessEqual(x: Double, y: Double): Boolean = x <= y
  def doubleGreater(x: Double, y: Double): Boolean = x > y
  def doubleGreaterEqual(x: Double, y: Double): Boolean = x >= y
  def doubleEqual(x: Double, y: Double): Boolean = x == y
  def doubleNotEqual(x: Double, y: Double): Boolean = x != y

  def if_[T](x: Boolean)(y: =>T)(z: =>T): T = if (x) y else z

}

trait Base_Str extends Base {

  case class Rep[+T](s: String) { override def toString = s; def +(s: String) = toString+s }

  var nSyms = 0
  def fresh = { nSyms += 1; "x" + (nSyms - 1) }

  def reflect[T](s: String): Rep[T] = { val x = fresh; println("val "+x+" = "+s); Rep(x) }
  def reify[T](x: => Rep[T]): String = "{" + captureOutput(x.s) + "}"

  import java.io._
  def captureOutput(func: => Unit): String = {
    val bstream = new ByteArrayOutputStream
    withOutput(new PrintStream(bstream))(func)
    bstream.toString
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


  implicit def unit(x: Boolean): Rep[Boolean] = Rep(x.toString)
  implicit def unit(x: Byte): Rep[Byte] = Rep(x.toString)
  implicit def unit(x: Char): Rep[Char] = Rep(x.toString)
  implicit def unit(x: Short): Rep[Short] = Rep(x.toString)
  implicit def unit(x: Int): Rep[Int] = Rep(x.toString)
  implicit def unit(x: Long): Rep[Long] = Rep(x.toString)
  implicit def unit(x: Float): Rep[Float] = Rep(x.toString)
  implicit def unit(x: Double): Rep[Double] = Rep(x.toString)


  def unit(x: Null): Rep[Object] = Rep("null")
  def unit(x: Object): Rep[Object] = Rep(x.toString)


  def byteToInt(x: Rep[Byte]): Rep[Int] = reflect(x+".toInt")
  def charToInt(x: Rep[Char]): Rep[Int] = reflect(x+".toInt")
  def shortToInt(x: Rep[Short]): Rep[Int] = reflect(x+".toInt")


  def intToByte(x: Rep[Int]): Rep[Byte] = reflect(x+".toByte")
  def intToChar(x: Rep[Int]): Rep[Char] = reflect(x+".toChar")
  def intToShort(x: Rep[Int]): Rep[Short] = reflect(x+".toShort")
  def intToInt(x: Rep[Int]): Rep[Int] = reflect(x+".toInt")
  def intToLong(x: Rep[Int]): Rep[Long] = reflect(x+".toLong")
  def intToFloat(x: Rep[Int]): Rep[Float] = reflect(x+".toFloat")
  def intToDouble(x: Rep[Int]): Rep[Double] = reflect(x+".toDouble")

  def intNegate(x: Rep[Int]): Rep[Int] = reflect("-"+x)
  def intPlus(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" + "+y)
  def intMinus(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" - "+y)
  def intTimes(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" * "+y)
  def intDiv(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" / "+y)
  def intMod(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" % "+y)
  def intAnd(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" & "+y)
  def intOr(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" | "+y)
  def intXor(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" ^ "+y)
  def intShiftLeft(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" << "+y)
  def intShiftRight(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" >> "+y)
  def intShiftRightUnsigned(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect(x+" >>> "+y)
  def intLess(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x+" < "+y)
  def intLessEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x+" <= "+y)
  def intGreater(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x+" > "+y)
  def intGreaterEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x+" >= "+y)
  def intEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x+" == "+y)
  def intNotEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect(x+" != "+y)

  def longToByte(x: Rep[Long]): Rep[Byte] = reflect(x+".toByte")
  def longToChar(x: Rep[Long]): Rep[Char] = reflect(x+".toChar")
  def longToShort(x: Rep[Long]): Rep[Short] = reflect(x+".toShort")
  def longToInt(x: Rep[Long]): Rep[Int] = reflect(x+".toInt")
  def longToLong(x: Rep[Long]): Rep[Long] = reflect(x+".toLong")
  def longToFloat(x: Rep[Long]): Rep[Float] = reflect(x+".toFloat")
  def longToDouble(x: Rep[Long]): Rep[Double] = reflect(x+".toDouble")

  def longNegate(x: Rep[Long]): Rep[Long] = reflect("-"+x)
  def longPlus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" + "+y)
  def longMinus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" - "+y)
  def longTimes(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" * "+y)
  def longDiv(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" / "+y)
  def longMod(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" % "+y)
  def longAnd(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" & "+y)
  def longOr(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" | "+y)
  def longXor(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" ^ "+y)
  def longShiftLeft(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" << "+y)
  def longShiftRight(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" >> "+y)
  def longShiftRightUnsigned(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect(x+" >>> "+y)
  def longLess(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" < "+y)
  def longLessEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" <= "+y)
  def longGreater(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" > "+y)
  def longGreaterEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" >= "+y)
  def longEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" == "+y)
  def longNotEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect(x+" != "+y)

  def floatToByte(x: Rep[Float]): Rep[Byte] = reflect(x+".toByte")
  def floatToChar(x: Rep[Float]): Rep[Char] = reflect(x+".toChar")
  def floatToShort(x: Rep[Float]): Rep[Short] = reflect(x+".toShort")
  def floatToInt(x: Rep[Float]): Rep[Int] = reflect(x+".toInt")
  def floatToLong(x: Rep[Float]): Rep[Long] = reflect(x+".toLong")
  def floatToFloat(x: Rep[Float]): Rep[Float] = reflect(x+".toFloat")
  def floatToDouble(x: Rep[Float]): Rep[Double] = reflect(x+".toDouble")

  def floatNegate(x: Rep[Float]): Rep[Float] = reflect("-"+x)
  def floatPlus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" + "+y)
  def floatMinus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" - "+y)
  def floatTimes(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" * "+y)
  def floatDiv(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" / "+y)
  def floatMod(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect(x+" % "+y)
  def floatLess(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" < "+y)
  def floatLessEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" <= "+y)
  def floatGreater(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" > "+y)
  def floatGreaterEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" >= "+y)
  def floatEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" == "+y)
  def floatNotEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect(x+" != "+y)

  def doubleToByte(x: Rep[Double]): Rep[Byte] = reflect(x+".toByte")
  def doubleToChar(x: Rep[Double]): Rep[Char] = reflect(x+".toChar")
  def doubleToShort(x: Rep[Double]): Rep[Short] = reflect(x+".toShort")
  def doubleToInt(x: Rep[Double]): Rep[Int] = reflect(x+".toInt")
  def doubleToLong(x: Rep[Double]): Rep[Long] = reflect(x+".toLong")
  def doubleToFloat(x: Rep[Double]): Rep[Float] = reflect(x+".toFloat")
  def doubleToDouble(x: Rep[Double]): Rep[Double] = reflect(x+".toDouble")

  def doubleNegate(x: Rep[Double]): Rep[Double] = reflect("-"+x)
  def doublePlus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" + "+y)
  def doubleMinus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" - "+y)
  def doubleTimes(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" * "+y)
  def doubleDiv(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" / "+y)
  def doubleMod(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect(x+" % "+y)
  def doubleLess(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" < "+y)
  def doubleLessEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" <= "+y)
  def doubleGreater(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" > "+y)
  def doubleGreaterEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" >= "+y)
  def doubleEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" == "+y)
  def doubleNotEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect(x+" != "+y)


  def if_[T](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T] =
    reflect("if ("+x+") "+reify(y)+" else "+reify(z))

}



