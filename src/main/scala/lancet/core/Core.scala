package lancet.core

trait Core extends Base {


  implicit def booleanType: TypeRep[Boolean]
  implicit def intType: TypeRep[Int]
  implicit def unitType: TypeRep[Unit]

  //implicit def anyType[T:Manifest]: TypeRep[T]



  implicit def unit(x: Boolean): Rep[Boolean]
  implicit def unit(x: Byte): Rep[Byte]
  implicit def unit(x: Char): Rep[Char]
  implicit def unit(x: Short): Rep[Short]
  implicit def unit(x: Int): Rep[Int]
  implicit def unit(x: Long): Rep[Long]
  implicit def unit(x: Float): Rep[Float]
  implicit def unit(x: Double): Rep[Double]

  // TODO: String?

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


  implicit def booleanOps(x: Rep[Boolean]) = new BooleanOps(x)
  class BooleanOps(x: Rep[Boolean]) { // TODO: somewhat crude
    def unary_! : Rep[Boolean] = if_(x) (unit(false)) (unit(true))
    def ||(y: =>Rep[Boolean]): Rep[Boolean] = if_(x) (x) (y)
    def &&(y: =>Rep[Boolean]): Rep[Boolean] = if_(x) (y) (x)
  }


  implicit def objectOps(x: Rep[Object]) = new ObjectOps(x)
  class ObjectOps(x: Rep[Object]) {
    def ===(y: Rep[Object]): Rep[Boolean] = objectEqual(x,y)
    def !==(y: Rep[Object]): Rep[Boolean] = objectNotEqual(x,y)
    def asInstanceOfRep[T:TypeRep]: Rep[T] = objectAsInstanceOf[T](x)
  }

  def objectEqual(x: Rep[Object], y: Rep[Object]): Rep[Boolean]
  def objectNotEqual(x: Rep[Object], y: Rep[Object]): Rep[Boolean]
  def objectAsInstanceOf[T:TypeRep](x: Rep[Object]): Rep[T]
  def objectIsInstanceOf[T:TypeRep](x: Rep[Object]): Rep[Boolean]

  def if_[T:TypeRep](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T]

}