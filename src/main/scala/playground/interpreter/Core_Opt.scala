package playground.interpreter

trait Base_Opt extends Base_Str {

  abstract class Rep[+T] { def +(s: String) = toString+s }

  case class Static[+T](x: T) extends Rep[T] { override def toString = "" + x }
  case class Dyn[+T](s: String) extends Rep[T] { override def toString = s }

  def repManifest[T:Manifest]: Manifest[Rep[T]] = manifest[Rep[T]]

  var nSyms = 0
  def fresh = { nSyms += 1; "x" + (nSyms - 1) }

  def emit(s: String) = println("          "+s)

  def reflect[T](s: Any*): Rep[T] = { 
    val rhs = s.mkString("")
    exprs.getOrElse(rhs, {
      val x = fresh; emit("val "+x+" = "+s.mkString("")); Dyn[T](x)
    }).asInstanceOf[Rep[T]]
  }
  def reify[T](x: => Rep[T]): String = "{" + captureOutput(x) + "}"

  def rewrite(s: String, x: Rep[Any]): Unit = {
    exprs += (s -> x)
  }


  abstract class Val[+T]
  case class Const[+T](x: T) extends Val[T]
  case class Partial[+T](fields: Map[String, Rep[Any]]) extends Val[T]
  case object Top extends Val[Nothing]

  var exprs: Map[String, Rep[Any]] = Map.empty

  var store: Map[String, Val[Any]] = Map.empty

  // TODO: track const modifications through the store, too

  def eval[T](x: Rep[T]): Val[T] = x match {
    case Static(x) => Const(x)
    case Dyn(s) => store.getOrElse(s, Top).asInstanceOf[Val[T]]
  }



  import java.io._
  def captureOutput(func: => Any): String = {
    val bstream = new ByteArrayOutputStream
    val r = withOutput(new PrintStream(bstream))(func)
    bstream.toString + r
  }
  def withOutput(out: PrintStream)(func: => Any): Any = {
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




trait Core_Opt extends Base_Opt with Core_Str {

  def liftConst[T](x:T): Rep[T] = Static(x)

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
    case _ => super.objectNotEqual(x,y)
  }

  override def if_[T](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T] = eval(x) match {
    case Const(x) => if (x) y else z
    case _ => super.if_(x)(y)(z)
  }
}