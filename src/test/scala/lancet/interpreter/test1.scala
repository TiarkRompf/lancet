package lancet
package interpreter

import lancet.api._

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

class TestInterpreter1 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-1"

  // interpret
  def testA = withOutFileChecked(prefix+"A") {

    class Foo {
      def bar(x: Int) = { println("hello: "+x); 9 }
    }

    val o = new Foo

    val runtime = HotSpotGraalRuntime.graalRuntime().getRuntime();

    val cls = o.getClass
    val reflectMeth = cls.getDeclaredMethod("bar", classOf[Int])
    val method = runtime.lookupJavaMethod(reflectMeth)

    val it = Lancet.newInterpreter
    it.TRACE = true
    it.TRACE_BYTE_CODE = true

    val res = it.execute(method, Array[Object](o, 8:Integer))
    
    println("res: " + res)
  }

/*
  // compile simple
  def testB = withOutFileChecked(prefix+"B") {

    final class Foo { // making class final -- Simple compiler can't resolve call otherwise 
      def bar(x: Int) = { System.out.println("hello: "+x); 9 }
    }

    val o = new Foo
    val it = Lancet.newCompilerSimple

    val f = it.compile((x:Int) => o.bar(8))
    println(f(7))

  }
*/
  

/*
  this takes quite long. profiling data: 73s / 2013-01-30
    total             73s
    allLubs           23s
    String.replace    19s
    contextKey        10s
    compile           7s
*/

/*
  // TODO: revisit once we have a better handle on path dependent conditionals

  // compile optimized
  def testC = withOutFileChecked(prefix+"C") {
    //assert(false) // crashes the vm at the moment

    class Foo {
      def bar(x: Int) = { System.out.println("hello: "+x); 9 }
    }

    val o = new Foo
    val it = Lancet.newCompilerOpt

    //it.emitControlFlow = false

    val f = it.compile((x:Int) => o.bar(8))
    println(f(7))
  }
*/

}