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

object Util {

  val runtime = HotSpotGraalRuntime.getInstance().getRuntime();
  val compiler = HotSpotGraalRuntime.getInstance().getCompiler();

  def topScope[A](body: => A) = {
    //val hotspotDebugConfig = new HotSpotDebugConfig(GraalOptions.Log + ",Escape", GraalOptions.Meter, GraalOptions.Time, GraalOptions.Dump, GraalOptions.MethodFilter, System.out)
    val hotspotDebugConfig = new HotSpotDebugConfig(GraalOptions.Log, GraalOptions.Meter, GraalOptions.Time, GraalOptions.Dump, GraalOptions.MethodFilter, System.out)
    Debug.setConfig(hotspotDebugConfig)
    Debug.scope("Playground", new Callable[A] {
        def call: A = {
          body
        }
    });
  }

  def phase(f: StructuredGraph => Unit) = new Phase { 
    def run(graph: StructuredGraph) = f(graph)
  }

  def printGraph(s: String) = phase { graph =>
    println("===== " + s)
    graph.getNodes.foreach(n => println(n.toString(Node.Verbosity.Short) + n.inputs().map(_.toString(Node.Verbosity.Id)).mkString("(",",",")")))
    println("----- " + s + " method calls ")
    graph.getNodes(classOf[InvokeNode]).foreach(printInvoke)
  }

  def printInvoke(invoke: InvokeNode): Unit = {
    val methodCallTarget = invoke.callTarget()
    val targetMethod = methodCallTarget.targetMethod() // ResolvedJavaMethod
    
    println("  invoke: " + invoke)
    println("    trgt: " + targetMethod)
    println("    args: " + methodCallTarget.arguments())
    
    /*val rcv = methodCallTarget.receiver()
    rcv match {
      case loadField: LoadFieldNode =>
        println("    rcv : " + loadField)
        println("          " + loadField.`object`)
      case _ =>
    }*/
    
    val assumptions = new Assumptions()
    val callback = new InliningUtil.InliningCallback {
      def buildGraph(method: ResolvedJavaMethod): StructuredGraph = error("not implemented")
      def inliningWeight(caller: ResolvedJavaMethod, method: ResolvedJavaMethod, invoke: Invoke): Double = 0.0
      def recordMethodContentsAssumption(method: ResolvedJavaMethod): Unit = {}
      def recordConcreteMethodAssumption(method: ResolvedJavaMethod, context: ResolvedJavaType, impl: ResolvedJavaMethod): Unit = {}
    }
    
    val info = InliningUtil.getInlineInfo(methodCallTarget.invoke(), 0, runtime, assumptions, callback, OptimisticOptimizations.ALL)
    println("    info: " + info)
  }

}

