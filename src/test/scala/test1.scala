object Helper {
  val foo = (x:Int) => 3*x
}

class Test1 extends FileDiffSuite {

  val prefix = "test-out/test1-"

  // compiled function references other function object (stored in external field)

  def testADefault = withOutFileChecked(prefix+"A-default") {

    // Default tries to inline .apply first but fails because
    // the exact receiver type is unknown (just Function1).
    // Then .foo is inlined, which exposes the exact type and thus
    // would enable inlining .apply, but .apply is never tried again.
    
    // The result has the .apply call remaining.
    /*
    ===== FINAL
    0|StartNode(3)
    2|Local(1)()
    3|FrameState@0(2)
    9|MethodCallTarget(17,2)
    10|Invoke#apply$mcII$sp(11,9)
    11|FrameState@12(10)
    14|Return(10)
    17|Const(Test1AHelper$.anonfun$1@1210959647)()
    18|FrameState@7()
    ----- FINAL method calls 
      invoke: 10|Invoke#apply$mcII$sp
        trgt: HotSpotMethod<Test1AHelper$$anonfun$1.apply$mcII$sp>
        args: [17|Const(Test1AHelper$.anonfun$1@1210959647), 2|Local(1)]
        info: exact Test1AHelper$$anonfun$1.apply$mcII$sp(int):int
    */
    
    val f = SimpleCompiler.compile((x:Int) => Helper.foo(x))
    
    println(f)
    println(f(7))
  }

  def testAOpt = withOutFileChecked(prefix+"A-opt") {

    // Repeated inlining unfolds .foo first and then .apply.

    // The result is just 3*x, all method calls are gone.
    /*
    ===== FINAL
    0|StartNode(3)
    2|Local(1)()
    3|FrameState@0(2)
    14|Return(22)
    19|Const(3)()
    22|*(2,19)
    ----- FINAL method calls
    */
    
    val f = Compiler.compile((x:Int) => Helper.foo(x))
    
    println(f)
    println(f(7))
  }


  // compiled function references other function object (stored in local, becomes field of closure)
  
  def testBDefault = withOutFileChecked(prefix+"B-default") {

    // Due to Scala's closure conversion, value foo is referenced 
    // through a field in the function object being compiled.
    
    // The default can not inspect this object because it only
    // sees its class' code, therefore it generates a field
    // access and a .apply call. 
    /*
    ===== FINAL
    0|StartNode(3)
    1|Local(0)()
    2|Local(1)()
    3|FrameState@0(1,2)
    5|MethodCallTarget(18,2)
    6|Invoke#apply$mcII$sp(7,5,16)
    7|FrameState@10(6)
    10|Return(6)
    11|Location()
    15|IsNull(18)
    16|!Guard(15,0)
    18|FloatingRead(1,11,0)
    19|FrameState@5()
    ----- FINAL method calls 
      invoke: 6|Invoke#apply$mcII$sp
        trgt: HotSpotMethod<Function1.apply$mcII$sp>
        args: [18|FloatingRead, 2|Local(1)]
        info: null
    */

    val foo = (x:Int) => 3*x
    val f = SimpleCompiler.compile((x:Int) => foo(x))
    
    println(f)
    println(f(7))
  }

  def testBOpt = withOutFileChecked(prefix+"B-opt") {

    // Replacing `this` references with Const nodes makes the
    // actual closure object accessible.
    
    // The compiled result is just 3*x, all method calls are gone.
    /*
    ===== FINAL
    0|StartNode(3)
    2|Local(1)()
    3|FrameState@0(11,2)
    10|Return(16)
    11|Const(Test1.$anonfun$testBOpt$1$$anonfun$7@1044020003)()
    13|Const(3)()
    16|*(2,13)
    ----- FINAL method calls 
    */
    
    val foo = (x:Int) => 3*x
    val f = Compiler.compile((x:Int)=>foo(x))
    
    println(f)
    println(f(7))
  }

}
