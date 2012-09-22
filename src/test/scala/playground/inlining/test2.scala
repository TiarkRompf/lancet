class Test2 extends FileDiffSuite {

  val prefix = "test-out/test2-"

  def testADefault = withOutFileChecked(prefix+"A-default") {

    // The .sum call is tried for inlining early but fails
    // (exact receiver unknown).
    // It is never tried again even though inlining other
    // calls exposes the receiver.
    
    // The compiled result still contains the .sum call.
    /*
    ----- FINAL method calls 
      invoke: 16|Invoke#sum
        trgt: HotSpotMethod<Range.sum>
        args: [308|Phi(310 335), 14|Const(Numeric.IntIsIntegral$@1005625829)]
        info: exact scala.collection.immutable.Range.sum(Numeric):Object
    */
    
    val f = SimpleCompiler.compile((x:Int) => (0 until x).sum)
    
    println(f)
    println(f(7))
    
  }

  def testAOpt = withOutFileChecked(prefix+"A-opt") {

    // Repeated inlining inlines .sum and a lot of
    // its internal method calls.
    
    // However there is another phase ordering
    // problem: Some calls like
    //    Function2.apply  [628|LoadField#op$3, 630|LoadField#elem, 622|Phi(613 615)]
    // do not have exact receivers at phase HIGH_LEVEL (they appear polymorphic there)
    // but become monomorphic at phase MID_LEVEL.
    //    TraversableOnce$$anonfun$sum$1.apply  [7012|Phi(7014 7231), 7379|FloatingRead, 622|Phi(7340 7162)]

    // The result is that the final code still has .apply method calls,
    // and at the end their receiver types are known:
    /*
    ----- FINAL method calls 
      invoke: 645|Invoke#apply
        trgt: HotSpotMethod<TraversableOnce$$anonfun$sum$1.apply>
        args: [7051|Phi(7053 7270), 7418|FloatingRead, 635|Phi(7379 7201)]
        info: exact scala.collection.TraversableOnce$$anonfun$sum$1.apply(Object, Object):Object
      invoke: 693|Invoke#apply
        trgt: HotSpotMethod<TraversableOnce$$anonfun$sum$1.apply>
        args: [7051|Phi(7053 7270), 7430|FloatingRead, 683|Phi(7393 7250)]
        info: exact scala.collection.TraversableOnce$$anonfun$sum$1.apply(Object, Object):Object
      invoke: 6818|Invoke#apply
        trgt: HotSpotMethod<TraversableOnce$$anonfun$sum$1.apply>
        args: [7051|Phi(7053 7270), 7373|FloatingRead, 7372|FloatingRead]
        info: exact scala.collection.TraversableOnce$$anonfun$sum$1.apply(Object, Object):Object
    */

    val f = Compiler.compile((x:Int) => (0 until x).sum)
    
    println(f)
    println(f(7))
    
  }


}
