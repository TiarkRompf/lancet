package playground
package staging

class TestStaging4 extends FileDiffSuite {

  val prefix = "test-out/test4-"

  def testA = withOutFileChecked(prefix+"A") {

    import Staging._

    def power(b: Code[Int], x: Int): Code[Int] =  { // use `eval`
      println("static: power " + x)
      if (x == 0) quote(1) else {
        val r = power(b, x-1)
        quote(eval(b) * eval(r))
      }
    }
    
    println("--- static")
    
    val pow4 = compile { b: Code[Int] => power(b, 4) }

    println("--- dynamic")
    
    val y = pow4(2)
    
    println(y)
    
  }


  def testB = withOutFileChecked(prefix+"B") {

    import Staging._    

    def power(b: Code[Int], x: Int): Code[Int] =  { // use `unquote`
      println("static: power " + x)
      if (x == 0) quote(1) else quote(unquote(b) * unquote(power(b, x-1)))
    }
    
    println("--- static")
    
    val pow4 = compile { b: Code[Int] => power(b, 4) }

    println("--- dynamic")
    
    val y = pow4(2)
    
    println(y)
    
  }


}