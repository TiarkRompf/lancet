package lancet
package analysis

class TestAnalysis7 extends FileDiffSuite {

  val prefix = "test-out/test-analysis-7"

/* 
  definitional interpreters for pcf
*/

  object Test1 {

    // *** run loop

    trait PCF {
      type Rep[T]

      def nat(c: Int): Rep[Int]
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int]
      def times(x: Rep[Int], y: Rep[Int]): Rep[Int]
      def ifz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T]

      def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B]
      def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B]
      def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B]

      // let fix f = ( let rec self n = f self n in self )

    }

    trait PCF_Direct extends PCF {
      type Rep[T] = T

      def nat(c: Int): Rep[Int] = c
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int] = x + y
      def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = x * y
      def ifz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T] = if (c == 0) a else b

      def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B] = f
      def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = f(x)
      def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = { def self(x:A):B = f(self)(x); self }

      // let fix f = ( let rec self n = f self n in self )
    }

    trait PCF_String extends PCF {
      type Rep[T] = String

      var nVars = 0
      def fresh[A] = { nVars += 1; "x"+(nVars-1) }


      def nat(c: Int): Rep[Int] = s"$c"
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int] = s"$x + $y"
      def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = s"$x * $y"
      def ifz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T] = s"if ($c == 0) { $a } else { $b }"

      def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B] = { 
        val x = fresh[A]; s"{ $x => ${f(x)} }" 
      }
      def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = s"$f($x)"
      def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = { 
        val self = fresh[A=>B]
        s"def $self:A=>B = ${f(self)}; $self"
      }

      // let fix f = ( let rec self n = f self n in self )
    }

    trait PCF_Env extends PCF {
      type Rep[T] = String

      var nVars = 0
      def fresh[A] = { nVars += 1; "f"+(nVars-1) }


      def nat(c: Int): Rep[Int] = s"$c"
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int] = s"$x + $y"
      def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = s"$x * $y"
      def ifz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T] = s"if ($c == 0) { $a } else { $b }"


/*
      type Env
      var elen = 0
      def envGet[A](e: Env, n: Int): A = s"$e($n)"
      def envWith[A,B](e: Env, x: Rep[A])(body: Rep[A]=>Rep[B]): Rep[B] = {
        elen += 1
        val r = s"$e($n)"
        elen -= 1
      }

      def mkFun(f: Env => Rep[B]): Rep[A=>B]
*/

      var envSize = 0

      def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B] = {
        val x = fresh[A];
        println(s"def $x(f,e) = ${ envSize += 1; val r = f("e._"+envSize.toString); envSize -= 1; r }")

        //val g = mkFun(e => envWith(f(envGetLast(e)))



        s"Clos($x,e)"
      }
      def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = s"$f.f($f.e :+ $x)"
      def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = { 
        f("Clos(f,e)")
      }

      // let fix f = ( let rec self n = f self n in self )
    }



    def run[T](f: => T): Unit = {

    }

   }


  // *** test 

  def testA = withOutFileChecked(prefix+"A") {
    import Test1._

    val eval: PCF = new PCF_Direct {}
    val comp: PCF = new PCF_String {}

    def df(IR: PCF) = {
      import IR._
      val fac = fix[Int,Int] { fac => lam[Int,Int] { n => 
        ifz(n, nat(1), times(n, app(fac, plus(n,nat(-1)))))
      }}
      app(fac,nat(4))
    }

    printcheck(df(eval),24)

    println(df(new PCF_String {}))
    println(df(new PCF_Env {}))
  }


}