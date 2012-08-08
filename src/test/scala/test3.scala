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

class Test3 extends FileDiffSuite {

  val prefix = "test-out/test3-"

  def testADefault = withOutFileChecked(prefix+"A-default") {

    val f = SimpleCompiler.compile { x: Int =>
      val a = new A(new C)
      a.foo.bar      
    }
    
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
