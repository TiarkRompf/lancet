package lancet.core

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


  def indented(s: String) = "  "+s.replace("\n","\n  ")

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
