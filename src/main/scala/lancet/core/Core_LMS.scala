package lancet.core

import scala.virtualization.lms.common._


trait Core_LMS extends Base_LMS {

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


  def reflect[T:TypeRep](d: Def[T]) = toAtom(d)


  case class PrimConvert[A:TypeRep,B:TypeRep](x:Rep[A]) extends Def[B]

  case class PrimNegate[A:TypeRep](x: Rep[A]) extends Def[A]
  case class PrimPlus[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimMinus[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimTimes[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimDiv[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimMod[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimAnd[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimOr[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimXor[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimShiftLeft[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimShiftRight[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimShiftRightUnsigned[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[A]
  case class PrimLess[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[Boolean]
  case class PrimLessEqual[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[Boolean]
  case class PrimGreater[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[Boolean]
  case class PrimGreaterEqual[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[Boolean]
  case class PrimEqual[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[Boolean]
  case class PrimNotEqual[A:TypeRep](x: Rep[A], y: Rep[A]) extends Def[Boolean]

  case class ObjectEqual(x: Rep[Object], y: Rep[Object]) extends Def[Boolean]
  case class ObjectNotEqual(x: Rep[Object], y: Rep[Object]) extends Def[Boolean]
  case class ObjectAsInstanceOf[T:TypeRep](x: Rep[Object]) extends Def[T]
  case class ObjectIsInstanceOf[T:TypeRep](x: Rep[Object]) extends Def[Boolean]

  case class IfThenElse[T:TypeRep](x: Rep[Boolean], y: Block[T], z: Block[T]) extends Def[Boolean]


  override def mirrorDef[A:TypeRep](d: Def[A], f: Transformer): Def[A] = (d match {
    case PrimConvert(x)                   => PrimConvert(f(x))(typeRep[Any], typeRep[A]) /// FIXME!!!! ---> GADTS

    case PrimNegate(x)                    => PrimNegate(f(x))
    case PrimPlus(x, y)                   => PrimPlus(f(x), f(y))
    case PrimMinus(x, y)                  => PrimMinus(f(x), f(y))
    case PrimTimes(x, y)                  => PrimTimes(f(x), f(y))
    case PrimDiv(x, y)                    => PrimDiv(f(x), f(y))
    case PrimMod(x, y)                    => PrimMod(f(x), f(y))
    case PrimAnd(x, y)                    => PrimAnd(f(x), f(y))
    case PrimOr(x, y)                     => PrimOr(f(x), f(y))
    case PrimXor(x, y)                    => PrimXor(f(x), f(y))
    case PrimShiftLeft(x, y)              => PrimShiftLeft(f(x), f(y))
    case PrimShiftRight(x, y)             => PrimShiftRight(f(x), f(y))
    case PrimShiftRightUnsigned(x, y)     => PrimShiftRightUnsigned(f(x), f(y))
    case PrimLess(x, y)                   => PrimLess(f(x), f(y))
    case PrimLessEqual(x, y)              => PrimLessEqual(f(x), f(y))
    case PrimGreater(x, y)                => PrimGreater(f(x), f(y))
    case PrimGreaterEqual(x, y)           => PrimGreaterEqual(f(x), f(y))
    case PrimEqual(x, y)                  => PrimEqual(f(x), f(y))
    case PrimNotEqual(x, y)               => PrimNotEqual(f(x), f(y))
    
    case ObjectEqual(x, y)                => ObjectEqual(f(x), f(y))
    case ObjectNotEqual(x, y)             => ObjectNotEqual(f(x), f(y))
    case ObjectAsInstanceOf(x)            => ObjectAsInstanceOf(f(x))(typeRep[A])
    case ObjectIsInstanceOf(x)            => ObjectIsInstanceOf(f(x))(typeRep[A]) // <------- NOT A!!

    case IfThenElse(x, y, z)              => IfThenElse(f(x), f(y), f(z))

    case _ => super.mirrorDef(d, f)    
  }).asInstanceOf[Def[A]]

  override def quickString[A:TypeRep](d: Def[A]): String = d match {
    case PrimConvert(x)                   => x+".to"+typeRep[A]

    case PrimNegate(x)                    => "-"+x
    case PrimPlus(x, y)                   => x+" + "+y
    case PrimMinus(x, y)                  => x+" - "+y
    case PrimTimes(x, y)                  => x+" * "+y
    case PrimDiv(x, y)                    => x+" / "+y
    case PrimMod(x, y)                    => x+" % "+y
    case PrimAnd(x, y)                    => x+" & "+y
    case PrimOr(x, y)                     => x+" | "+y
    case PrimXor(x, y)                    => x+" ^ "+y
    case PrimShiftLeft(x, y)              => x+" << "+y
    case PrimShiftRight(x, y)             => x+" >> "+y
    case PrimShiftRightUnsigned(x, y)     => x+" >>> "+y
    case PrimLess(x, y)                   => x+" < "+y
    case PrimLessEqual(x, y)              => x+" <= "+y
    case PrimGreater(x, y)                => x+" > "+y
    case PrimGreaterEqual(x, y)           => x+" >= "+y
    case PrimEqual(x, y)                  => x+" == "+y
    case PrimNotEqual(x, y)               => x+" != "+y
    
    case ObjectEqual(x, y)                => x+" eq "+y
    case ObjectNotEqual(x, y)             => x+" ne "+y
    case ObjectAsInstanceOf(x)            => x+".asInstanceOf["+typeRep[A]+"]"
    case ObjectIsInstanceOf(x)            => x+".isInstanceOf["+typeRep[A]+"]"

    //case IfThenElse(x, y, z)              => do default
    case _ => super.quickString(d)
  }






/*
  override def mirror[T](d: Def[T]): Rep[T] = x match {
    case _ => super.mirror(x,d)
  }
*/


  def byteToInt(x: Rep[Byte]): Rep[Int] = reflect[Int](PrimConvert[Byte,Int](x))
  def charToInt(x: Rep[Char]): Rep[Int] = reflect[Int](PrimConvert[Char,Int](x))
  def shortToInt(x: Rep[Short]): Rep[Int] = reflect[Int](PrimConvert[Short,Int](x))


  def intToByte(x: Rep[Int]): Rep[Byte] = reflect[Byte](PrimConvert[Int,Byte](x))
  def intToChar(x: Rep[Int]): Rep[Char] = reflect[Char](PrimConvert[Int,Char](x))
  def intToShort(x: Rep[Int]): Rep[Short] = reflect[Short](PrimConvert[Int,Short](x))
  def intToInt(x: Rep[Int]): Rep[Int] = reflect[Int](PrimConvert[Int,Int](x))
  def intToLong(x: Rep[Int]): Rep[Long] = reflect[Long](PrimConvert[Int,Long](x))
  def intToFloat(x: Rep[Int]): Rep[Float] = reflect[Float](PrimConvert[Int,Float](x))
  def intToDouble(x: Rep[Int]): Rep[Double] = reflect[Double](PrimConvert[Int,Double](x))

  def intNegate(x: Rep[Int]): Rep[Int] = reflect[Int](PrimNegate[Int](x))
  def intPlus(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimPlus[Int](x,y))
  def intMinus(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimMinus[Int](x,y))
  def intTimes(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimTimes[Int](x,y))
  def intDiv(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimDiv[Int](x,y))
  def intMod(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimMod[Int](x,y))
  def intAnd(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimAnd[Int](x,y))
  def intOr(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimOr[Int](x,y))
  def intXor(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimXor[Int](x,y))
  def intShiftLeft(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimShiftLeft[Int](x,y))
  def intShiftRight(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimShiftRight[Int](x,y))
  def intShiftRightUnsigned(x: Rep[Int], y: Rep[Int]): Rep[Int] = reflect[Int](PrimShiftRightUnsigned[Int](x,y))
  def intLess(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](PrimLess[Int](x,y))
  def intLessEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](PrimLessEqual[Int](x,y))
  def intGreater(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](PrimGreater[Int](x,y))
  def intGreaterEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](PrimGreaterEqual[Int](x,y))
  def intEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](PrimEqual[Int](x,y))
  def intNotEqual(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = reflect[Boolean](PrimNotEqual[Int](x,y))

  def longToByte(x: Rep[Long]): Rep[Byte] = reflect[Byte](PrimConvert[Long,Byte](x))
  def longToChar(x: Rep[Long]): Rep[Char] = reflect[Char](PrimConvert[Long,Char](x))
  def longToShort(x: Rep[Long]): Rep[Short] = reflect[Short](PrimConvert[Long,Short](x))
  def longToInt(x: Rep[Long]): Rep[Int] = reflect[Int](PrimConvert[Long,Int](x))
  def longToLong(x: Rep[Long]): Rep[Long] = reflect[Long](PrimConvert[Long,Long](x))
  def longToFloat(x: Rep[Long]): Rep[Float] = reflect[Float](PrimConvert[Long,Float](x))
  def longToDouble(x: Rep[Long]): Rep[Double] = reflect[Double](PrimConvert[Long,Double](x))

  def longNegate(x: Rep[Long]): Rep[Long] = reflect[Long](PrimNegate[Long](x))
  def longPlus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimPlus[Long](x,y))
  def longMinus(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimMinus[Long](x,y))
  def longTimes(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimTimes[Long](x,y))
  def longDiv(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimDiv[Long](x,y))
  def longMod(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimMod[Long](x,y))
  def longAnd(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimAnd[Long](x,y))
  def longOr(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimOr[Long](x,y))
  def longXor(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimXor[Long](x,y))
  def longShiftLeft(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimShiftLeft[Long](x,y))
  def longShiftRight(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimShiftRight[Long](x,y))
  def longShiftRightUnsigned(x: Rep[Long], y: Rep[Long]): Rep[Long] = reflect[Long](PrimShiftRightUnsigned[Long](x,y))
  def longLess(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](PrimLess[Long](x,y))
  def longLessEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](PrimLessEqual[Long](x,y))
  def longGreater(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](PrimGreater[Long](x,y))
  def longGreaterEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](PrimGreaterEqual[Long](x,y))
  def longEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](PrimEqual[Long](x,y))
  def longNotEqual(x: Rep[Long], y: Rep[Long]): Rep[Boolean] = reflect[Boolean](PrimNotEqual[Long](x,y))

  def floatToByte(x: Rep[Float]): Rep[Byte] = reflect[Byte](PrimConvert[Float,Byte](x))
  def floatToChar(x: Rep[Float]): Rep[Char] = reflect[Char](PrimConvert[Float,Char](x))
  def floatToShort(x: Rep[Float]): Rep[Short] = reflect[Short](PrimConvert[Float,Short](x))
  def floatToInt(x: Rep[Float]): Rep[Int] = reflect[Int](PrimConvert[Float,Int](x))
  def floatToLong(x: Rep[Float]): Rep[Long] = reflect[Long](PrimConvert[Float,Long](x))
  def floatToFloat(x: Rep[Float]): Rep[Float] = reflect[Float](PrimConvert[Float,Float](x))
  def floatToDouble(x: Rep[Float]): Rep[Double] = reflect[Double](PrimConvert[Float,Double](x))

  def floatNegate(x: Rep[Float]): Rep[Float] = reflect[Float](PrimNegate[Float](x))
  def floatPlus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](PrimPlus[Float](x,y))
  def floatMinus(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](PrimMinus[Float](x,y))
  def floatTimes(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](PrimTimes[Float](x,y))
  def floatDiv(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](PrimDiv[Float](x,y))
  def floatMod(x: Rep[Float], y: Rep[Float]): Rep[Float] = reflect[Float](PrimMod[Float](x,y))
  def floatLess(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](PrimLess[Float](x,y))
  def floatLessEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](PrimLessEqual[Float](x,y))
  def floatGreater(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](PrimGreater[Float](x,y))
  def floatGreaterEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](PrimGreaterEqual[Float](x,y))
  def floatEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](PrimEqual[Float](x,y))
  def floatNotEqual(x: Rep[Float], y: Rep[Float]): Rep[Boolean] = reflect[Boolean](PrimNotEqual[Float](x,y))

  def doubleToByte(x: Rep[Double]): Rep[Byte] = reflect[Byte](PrimConvert[Double,Byte](x))
  def doubleToChar(x: Rep[Double]): Rep[Char] = reflect[Char](PrimConvert[Double,Char](x))
  def doubleToShort(x: Rep[Double]): Rep[Short] = reflect[Short](PrimConvert[Double,Short](x))
  def doubleToInt(x: Rep[Double]): Rep[Int] = reflect[Int](PrimConvert[Double,Int](x))
  def doubleToLong(x: Rep[Double]): Rep[Long] = reflect[Long](PrimConvert[Double,Long](x))
  def doubleToFloat(x: Rep[Double]): Rep[Float] = reflect[Float](PrimConvert[Double,Float](x))
  def doubleToDouble(x: Rep[Double]): Rep[Double] = reflect[Double](PrimConvert[Double,Double](x))

  def doubleNegate(x: Rep[Double]): Rep[Double] = reflect[Double](PrimNegate[Double](x))
  def doublePlus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](PrimPlus[Double](x,y))
  def doubleMinus(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](PrimMinus[Double](x,y))
  def doubleTimes(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](PrimTimes[Double](x,y))
  def doubleDiv(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](PrimDiv[Double](x,y))
  def doubleMod(x: Rep[Double], y: Rep[Double]): Rep[Double] = reflect[Double](PrimMod[Double](x,y))
  def doubleLess(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](PrimLess[Double](x,y))
  def doubleLessEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](PrimLessEqual[Double](x,y))
  def doubleGreater(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](PrimGreater[Double](x,y))
  def doubleGreaterEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](PrimGreaterEqual[Double](x,y))
  def doubleEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] =  reflect[Boolean](PrimEqual[Double](x,y))
  def doubleNotEqual(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = reflect[Boolean](PrimNotEqual[Double](x,y))


  def objectEqual(x: Rep[Object], y: Rep[Object]): Rep[Boolean] = reflect[Boolean](ObjectEqual(x,y))
  def objectNotEqual(x: Rep[Object], y: Rep[Object]): Rep[Boolean] = reflect[Boolean](ObjectNotEqual(x,y))
  def objectAsInstanceOf[T:TypeRep](x: Rep[Object]): Rep[T] = reflect[T](ObjectAsInstanceOf[T](x))
  def objectIsInstanceOf[T:TypeRep](x: Rep[Object]): Rep[Boolean] = reflect[Boolean](ObjectIsInstanceOf[T](x))

  def if_[T:TypeRep](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T] = {
    //val save = exprs
    // TODO: state lub; reset exprs for both branches!
    var r = reflect[T](IfThenElse(x,reify(y),reify(z)))
    //exprs = save
    r
  }

}



trait ScalaGenCore extends GEN_Scala_LMS_Base {
  val IR: Core_LMS
  import IR._
 
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrimConvert(x)                   => emitValDef(sym, quote(x)+".to"+remap(sym.tp))

    case PrimNegate(x)                    => emitValDef(sym, "-"+x)
    case PrimPlus(x, y)                   => emitValDef(sym, x+" + "+y)
    case PrimMinus(x, y)                  => emitValDef(sym, x+" - "+y)
    case PrimTimes(x, y)                  => emitValDef(sym, x+" * "+y)
    case PrimDiv(x, y)                    => emitValDef(sym, x+" / "+y)
    case PrimMod(x, y)                    => emitValDef(sym, x+" % "+y)
    case PrimAnd(x, y)                    => emitValDef(sym, x+" & "+y)
    case PrimOr(x, y)                     => emitValDef(sym, x+" | "+y)
    case PrimXor(x, y)                    => emitValDef(sym, x+" ^ "+y)
    case PrimShiftLeft(x, y)              => emitValDef(sym, x+" << "+y)
    case PrimShiftRight(x, y)             => emitValDef(sym, x+" >> "+y)
    case PrimShiftRightUnsigned(x, y)     => emitValDef(sym, x+" >>> "+y)
    case PrimLess(x, y)                   => emitValDef(sym, x+" < "+y)
    case PrimLessEqual(x, y)              => emitValDef(sym, x+" <= "+y)
    case PrimGreater(x, y)                => emitValDef(sym, x+" > "+y)
    case PrimGreaterEqual(x, y)           => emitValDef(sym, x+" >= "+y)
    case PrimEqual(x, y)                  => emitValDef(sym, x+" == "+y)
    case PrimNotEqual(x, y)               => emitValDef(sym, x+" != "+y)
    
    case ObjectEqual(x, y)                => emitValDef(sym, x+" eq "+y)
    case ObjectNotEqual(x, y)             => emitValDef(sym, x+" ne "+y)
    case ObjectAsInstanceOf(x)            => emitValDef(sym, x+".asInstanceOf["+remap(sym.tp)+"]")
    case ObjectIsInstanceOf(x)            => emitValDef(sym, x+".isInstanceOf["+remap(sym.tp)+"]")

    case IfThenElse(x, y, z)              => stream.print("if ("+x+") ") // Unit result??
                                             emitBlockFull(y)
                                             Console.print(" else ")
                                             emitBlockFull(z)

    case _ => super.emitNode(sym,rhs)
  }

}
