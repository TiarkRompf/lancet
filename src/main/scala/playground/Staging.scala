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

case class Code[A](graph: StructuredGraph, method: ResolvedJavaMethod) {
  def ~ = Staging.eval(this)(manifest[Int].asInstanceOf[Manifest[A]])
}

object Staging {

  val runtime = HotSpotGraalRuntime.getInstance().getRuntime()
  val compiler = HotSpotGraalRuntime.getInstance().getCompiler()

  val config = new GraphBuilderConfiguration(GraphBuilderConfiguration.ResolvePolicy.Eager, null) // resolve eagerly, lots of DeoptNodes otherwise


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
  
  
  // replace `unquote` calls with the reflectively computed graph
  
  def unquoteMacro(self: Node, fun: Node, manif: Node): StructuredGraph = fun match {
    case fun: NewInstanceNode =>
      println("unquote")
      val cls = fun.instanceClass.toJava.asInstanceOf[Class[()=>Code[Any]]]
      // need to find constructor invocation...
      println("self: " + self)
      println("fun: " + fun + "/" + cls + "/" + fun.next())
      val init = fun.next().asInstanceOf[InvokeNode]
      val initCallTarget = init.callTarget()
      val initTarget = initCallTarget.targetMethod()
      assert(initTarget.name == "<init>")
      val initArgs = initCallTarget.arguments()
      println("init: " + init + " " + initArgs + " " + initArgs.map(_.kind()))
      val initOuter = initArgs(1).asInstanceOf[ConstantNode].value.boxedValue()
      
      // TODO: need to be more robust
      
      println(initTarget)
      println(initTarget.signature())
      println("--")
      
      cls.getConstructors.foreach(println)
      
      // all constructor arguments (free variables) need to be Code types!
      // (TODO: add check + error messages)
      // other types should be fine, too, if the args are constants.
      
      val a = Array.fill[Class[_]](initArgs.length-1)(classOf[Code[_]])
      a(0) = initOuter.getClass
      
      val ctor = cls.getConstructor(a:_*)
      
      def nodeToCode(n: ValueNode): Code[_] = { // does this work always?
        val graph = new StructuredGraph()
        val n2 = n.clone(graph).asInstanceOf[ValueNode]
        val ret = graph.add(new ReturnNode(n2))
        graph.start().setNext(ret)
        Code(graph, null)
      }
      
      val ctorArgs = initOuter +: initArgs.drop(2).map(nodeToCode)
      
      val Code(intrinsicGraph,meth) = ctor.newInstance(ctorArgs:_*).apply()
      intrinsicGraph
    //case _ => match error
  }


  // replace `eval` calls with the given constant graph
  
  def evalMacro(self: Node, code: Node, manif: Node): StructuredGraph = code match {
    case code: ConstantNode =>
      println("eval")
      println(code)

      val Code(intrinsicGraph,meth) = code.value.asObject()
      intrinsicGraph
    case _ =>
      println("warning: cannot inline non-constant code" + code)
      null
  }

  
  // macro expansion
  
  def doMacros(graph: StructuredGraph): Unit = {
    graph.getNodes(classOf[InvokeNode]) foreach { invoke => 
      val callTarget = invoke.callTarget()
      val target = callTarget.targetMethod()
      if (target != null) {
        val args = callTarget.arguments()
        val intrinsicGraph = target.name() match {
          case "unquote" => unquoteMacro(args(0), args(1), args(2))
          case "eval" => evalMacro(args(0), args(1), args(2))
          case _ => null
        }
        if (intrinsicGraph != null)
          InliningUtil.inline(invoke, intrinsicGraph, true);
      }
    }
    new CanonicalizerPhase(compiler.target, runtime, null /*assumptions*/).apply(graph)
  }
  
  

  // helpers
  
  def toFunction[A](x: => A) = ((x:()=>A)=>x).asInstanceOf[{def apply(x: => A):()=>A}](x)


  // return a code fragment representing the argument
  
  def quote[A:Manifest](x: => A): Code[A] = {
    assert(manifest[A] == manifest[Int]) // for now ...
    
    val f = toFunction(x)
    val cls = f.getClass
    val reflectMeth = cls.getDeclaredMethod("apply$mcI$sp")
    val method = runtime.getResolvedJavaMethod(reflectMeth)

    val graph = new StructuredGraph(method);
    val graphBuilderPhase = new GraphBuilderPhase(compiler.runtime, config, OptimisticOptimizations.ALL);
    graphBuilderPhase.apply(graph)

    doConstThis(f)(graph)
    doMacros(graph)
    
    Code(graph, method)
  }
  
  
  // splice in the code fragment returned by evaluating the argument
  
  //FIXME: return type should be A -- currently fixed to prevent type errors when replacing call
  def unquote[A:Manifest](code: => Code[A]): Int = {
    val f = toFunction(code)
    throw new Exception("cannot interpret unquote")
    
  }


  // evaluate / splice in the give code fragment
  
  //FIXME: return type should be A -- currently fixed to prevent type errors when replacing call
  def eval[A:Manifest](code: Code[A]): Int = {

    val Code(graph, method) = code

    val plan = new PhasePlan();
    val graphBuilderPhase = new GraphBuilderPhase(compiler.runtime, config, OptimisticOptimizations.ALL);
    plan.addPhase(PhasePosition.AFTER_PARSING, graphBuilderPhase); // adding it to the plan is required for inlining
    //plan.disablePhase(classOf[InliningPhase]) // turn off default inlining

    val result = Util.topScope {
      compiler.compileMethod(method, graph, -1, null, plan, OptimisticOptimizations.ALL)
    }
    
    Util.printGraph("EVAL")(graph)
    println("===== DONE")

    val compiledMethod = runtime.addMethod(method, result, null)
    
    println("after addMethod (" + method + "), now executing...")
    
    val y = compiledMethod.executeVarargs()
    y.asInstanceOf[Int]
  }


  // run compiler on argument closure

  var freshId = 0

  def compile[A:Manifest,B:Manifest](f: Code[A]=>Code[B]): A=>B = {
    assert(manifest[A] == manifest[Int] && manifest[B] == manifest[Int]) // for now ...
    
    // FIXME: should use proper substitution
    var _arg: Int = 0
    def getArg(id:Int) = _arg//throw new Exception("cannot interpret getArg")

    freshId += 1
    val id = freshId
    val arg = quote(getArg(id)).asInstanceOf[Code[A]]
    val res = f(arg)

    val Code(graph, method) = res

    // TODO: replace getArg(id) -> Local(1)

    val plan = new PhasePlan();
    val graphBuilderPhase = new GraphBuilderPhase(compiler.runtime, config, OptimisticOptimizations.ALL);
    plan.addPhase(PhasePosition.AFTER_PARSING, graphBuilderPhase); // adding it to the plan is required for inlining
    //plan.disablePhase(classOf[InliningPhase]) // turn off default inlining

    val result = Util.topScope {
      compiler.compileMethod(method, graph, -1, null, plan, OptimisticOptimizations.ALL)
    }
    
    Util.printGraph("EVAL")(graph)
    println("===== DONE")

    val compiledMethod = runtime.addMethod(method, result, null)
    
    { (x:A) => 
      _arg = x.asInstanceOf[Int]
      val y = compiledMethod.executeVarargs(f, x.asInstanceOf[AnyRef])
      y.asInstanceOf[B]
    }
  }

}