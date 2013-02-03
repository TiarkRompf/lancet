package lancet.interpreter



trait Base_Exec extends Base {
  type Rep[+T] = T
  def repManifest[T:Manifest]: Manifest[T] = manifest[T]
}



trait Core_Exec extends Base_Exec with Core {
  type TypeRep[T] = Manifest[T]
  implicit def anyType[T:Manifest]: TypeRep[T] = manifest[T]
  implicit def booleanType = manifest[Boolean]


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

  def objectEqual(x: Object, y: Object): Boolean = x eq y
  def objectNotEqual(x: Object, y: Object): Boolean = x ne y
  def objectAsInstanceOf[T:TypeRep](x: Object): T = x.asInstanceOf[T]

  def if_[T:TypeRep](x: Boolean)(y: =>T)(z: =>T): T = if (x) y else z

}

