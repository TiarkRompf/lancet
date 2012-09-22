package playground

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

object SimpleCompiler {

  val runtime = HotSpotGraalRuntime.getInstance().getRuntime();
  val compiler = HotSpotGraalRuntime.getInstance().getCompiler();

  val config = new GraphBuilderConfiguration(GraphBuilderConfiguration.ResolvePolicy.Eager, null) // resolve eagerly, lots of DeoptNodes otherwise

  // run default graal compiler on argument closure
  
  def compile[A:Manifest,B:Manifest](f: A=>B) = {
    assert(manifest[A] == manifest[Int] && manifest[B] == manifest[Int]) // for now ...
    
    val cls = f.getClass
    val reflectMeth = cls.getDeclaredMethod("apply$mcII$sp", classOf[Int])
    val method = runtime.getResolvedJavaMethod(reflectMeth)

    val graph = new StructuredGraph(method);
    val graphBuilderPhase = new GraphBuilderPhase(compiler.runtime, config, OptimisticOptimizations.ALL);
    graphBuilderPhase(graph)
    new DeadCodeEliminationPhase().apply(graph);
    
    Util.printGraph("AFTER_PARSING")(graph)

    val plan = new PhasePlan();
    plan.addPhase(PhasePosition.AFTER_PARSING, graphBuilderPhase); // adding it to the plan is required for inlining
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

