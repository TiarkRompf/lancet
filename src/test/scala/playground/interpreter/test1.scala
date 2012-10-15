package playground
package interpreter

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

class TestInterpreter1 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-1"

  def testA = withOutFileChecked(prefix+"A") {

    class Foo {
      def bar(x: Int) = println("hello: "+x)
    }

    val o = new Foo

    val runtime = HotSpotGraalRuntime.getInstance().getRuntime();
    val compiler = HotSpotGraalRuntime.getInstance().getCompiler();

    val cls = o.getClass
    val reflectMeth = cls.getDeclaredMethod("bar", classOf[Int])
    val method = runtime.getResolvedJavaMethod(reflectMeth)

    val it = new BytecodeInterpreter_Impl
    it.TRACE = true
    it.TRACE_BYTE_CODE = true
    it.initialize("")
    it.execute(method, Array[Object](o, 8:Integer))
    
  }


  def testB = withOutFileChecked(prefix+"B") {

    class Foo {
      def bar(x: Int) = println("hello: "+x)
    }

    val o = new Foo

    val runtime = HotSpotGraalRuntime.getInstance().getRuntime();
    val compiler = HotSpotGraalRuntime.getInstance().getCompiler();

    val cls = o.getClass
    val reflectMeth = cls.getDeclaredMethod("bar", classOf[Int])
    val method = runtime.getResolvedJavaMethod(reflectMeth)

    val it = new BytecodeInterpreter_Simple

    it.initialize("")
    it.execute(method, Array[Object](o, 8:Integer))    
  }

  def testC = withOutFileChecked(prefix+"C") {

    class Foo {
      def bar(x: Int) = println("hello: "+x)
    }

    val o = new Foo

    val runtime = HotSpotGraalRuntime.getInstance().getRuntime();
    val compiler = HotSpotGraalRuntime.getInstance().getCompiler();

    val cls = o.getClass
    val reflectMeth = cls.getDeclaredMethod("bar", classOf[Int])
    val method = runtime.getResolvedJavaMethod(reflectMeth)

    val it = new BytecodeInterpreter_Opt

    //it.emitControlFlow = false

    it.initialize("")
    it.execute(method, Array[Object](o, 8:Integer))    
  }

}