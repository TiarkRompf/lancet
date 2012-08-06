import java.util.concurrent.Callable

import com.oracle.graal.{java=>J,_}
import com.oracle.graal.debug._         // Debug
import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.api.code._      // Assumptions
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime
import com.oracle.graal.compiler._      // GraalCompiler
import com.oracle.graal.compiler.util._ // InliningUtil
import com.oracle.graal.compiler.types._// PropagateTypeCachePhase
import com.oracle.graal.java._          // GraphBuilderConfiguration
import com.oracle.graal.graph._
import com.oracle.graal.nodes.{java=>J,_}   // StructuredGraph
import com.oracle.graal.nodes.java._        // MethodCallTargetNode
import com.oracle.graal.compiler.phases._   // PhasePlan
import com.oracle.graal.compiler.phases.PhasePlan.PhasePosition

import collection.JavaConversions._

object Compiler {

  val runtime = HotSpotGraalRuntime.getInstance().getRuntime()
  val compiler = HotSpotGraalRuntime.getInstance().getCompiler()

  val config = new GraphBuilderConfiguration(GraphBuilderConfiguration.ResolvePolicy.Eager, null) // resolve eagerly, lots of DeoptNodes otherwise


  // inlining helpers

  def doInlineOne(graph: StructuredGraph): Boolean = {
    // TODO: actually record assumptions
    val assumptions = new Assumptions()
    
    val callback = new InliningUtil.InliningCallback {
      def buildGraph(method: ResolvedJavaMethod): StructuredGraph = {
        val graph = new StructuredGraph(method)
        val graphBuilderPhase = new GraphBuilderPhase(compiler.runtime, config, OptimisticOptimizations.ALL)
        graphBuilderPhase.apply(graph)
        graph
      }
      def inliningWeight(caller: ResolvedJavaMethod, method: ResolvedJavaMethod, invoke: Invoke): Double = 0.0
      def recordMethodContentsAssumption(method: ResolvedJavaMethod): Unit = {}
      def recordConcreteMethodAssumption(method: ResolvedJavaMethod, context: ResolvedJavaType, impl: ResolvedJavaMethod): Unit = {}
    }

    graph.getInvokes().foreach { invoke => 
      val methodCallTarget = invoke.callTarget()
      val targetMethod = methodCallTarget.targetMethod()
      
      println("  invoke:" + invoke)
      println("    target:" + targetMethod)
      println("    args:  " + methodCallTarget.arguments())
      
      val info = InliningUtil.getInlineInfo(invoke, 0, runtime, assumptions, callback, OptimisticOptimizations.ALL)
      if (info != null && info.getClass.getSimpleName.startsWith("Exact")) { //info.isInstanceOf[InliningUtil.ExactInlineInfo]
        println("  inlining:  " + info)
        info.inline(graph, runtime, callback)
        // what is the right set of phases to run?
        new PhiStampPhase().apply(graph)
        //new PropagateTypeCachePhase(compiler.target, runtime, assumptions).apply(graph)
        new CanonicalizerPhase(compiler.target, runtime, assumptions).apply(graph)
        new IntrinsificationPhase(runtime).apply(graph)
        
        return true
      }
    }
    false
  }
  
  def doInline(graph: StructuredGraph): Unit = {
    println("===== try inlining ...")
    if (doInlineOne(graph)) doInline(graph)
  }


  // replace `this` with constant receiver object
  
  def doConstThis(obj: Any)(graph: StructuredGraph): Unit = {
    if (!java.lang.reflect.Modifier.isStatic(graph.method().accessFlags())) {
        val receiver = graph.getLocal(0)
        if (receiver != null) {
            val cst = ConstantNode.forObject(obj, runtime, graph)
            receiver.replaceAndDelete(cst)
        }
    }
    new CanonicalizerPhase(compiler.target, runtime, null /*assumptions*/).apply(graph)
  }
  

  // run compiler on argument closure
  
  def compile[A:Manifest,B:Manifest](f: A=>B) = {
    assert(manifest[A] == manifest[Int] && manifest[B] == manifest[Int]) // for now ...
    
    val cls = f.getClass
    val reflectMeth = cls.getDeclaredMethod("apply$mcII$sp", classOf[Int])
    val method = runtime.getResolvedJavaMethod(reflectMeth)

    val graph = new StructuredGraph(method);
    val graphBuilderPhase = new GraphBuilderPhase(compiler.runtime, config, OptimisticOptimizations.ALL);
    graphBuilderPhase.apply(graph)

    Util.printGraph("AFTER_PARSING")(graph)

    doConstThis(f)(graph) // replace `this` with f
    doInline(graph)       // perform aggressive, iterated inlining

    Util.printGraph("AFTER_INLINING")(graph)

    val plan = new PhasePlan();
    //plan.addPhase(PhasePosition.AFTER_PARSING, graphBuilderPhase); // adding it to the plan is required for inlining
    plan.disablePhase(classOf[InliningPhase]) // turn off default inlining

    plan.addPhase(PhasePosition.HIGH_LEVEL, Util.printGraph("HIGH_LEVEL"))
    plan.addPhase(PhasePosition.MID_LEVEL, Util.printGraph("MID_LEVEL"))

    val result = Util.topScope {
      compiler.compileMethod(method, graph, -1, null, plan, OptimisticOptimizations.ALL)
    }
    
    Util.printGraph("FINAL")(graph)
    println("===== DONE")

    val compiledMethod = runtime.addMethod(method, result, null)
    
    { (x:A) => 
      val y = compiledMethod.executeVarargs(f, x.asInstanceOf[AnyRef])
      y.asInstanceOf[B]
    }
  }

}

