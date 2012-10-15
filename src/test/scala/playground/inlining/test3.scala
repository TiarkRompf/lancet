package playground
package inlining

class A(val foo: B)

class B {
  def bar = 1
}

class C extends B {
  override def bar = 2
}


// Narrowing down Test2: The problem is that LoadField is optimized only 
// for constants but not for newly created objects (more precisely, the
// stores executed in their constructors). Write propagation
// happens only later.

class TestInline3 extends FileDiffSuite {

  val prefix = "test-out/test3-"

  def testADefault = withOutFileChecked(prefix+"A-default") {

    val f = SimpleCompiler.compile { x: Int =>
      val a = new A(new C)
      a.foo.bar      
    }
    
    /*
    ----- HIGH_LEVEL method calls 
      invoke: 19|Invoke#bar
        trgt: HotSpotMethod<B.bar>
        args: [24|LoadField#foo]
        info: null
    ----- MID_LEVEL method calls 
      invoke: 19|Invoke#bar
        trgt: HotSpotMethod<C.bar>
        args: [87|Phi(89 103)]
        info: exact C.bar():int
    */
    
    println(f)
    println(f(7))
    
  }

  def testAOpt = withOutFileChecked(prefix+"A-opt") {

    val f = Compiler.compile { x: Int =>
      val a = new A(new C)
      a.foo.bar      
    }
    
    println(f)
    println(f(7))
    
  }

}
