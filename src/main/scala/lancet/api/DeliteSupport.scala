package lancet.api

/**
 * Copy and pasted from test/scala/lancet/advanced/DeliteSupport.scala!! Need to refactor.
 */

import lancet.api._
import lancet.interpreter._
import lancet.core._

import ppl.dsl.optiml.{Vector,DenseVector,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config

import ppl.dsl.optiml.{OptiMLCodeGenScala,OptiMLExp}
import ppl.delite.framework.codegen.delite.{DeliteCodeGenPkg, TargetDelite}
import ppl.delite.framework.codegen.{Target}
import ppl.delite.framework.codegen.scala.{TargetScala}
import scala.virtualization.lms.internal.{GenericFatCodegen}

import scala.virtualization.lms.common._
import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, SynchronizedBuffer }

import java.io.{ Console => _, _ }
import java.io.{File,FileSystem}


// *** integration class: takes the role of DeliteApplication

class LancetDeliteRunner extends LancetImpl
  with DeliteTestRunner with OptiMLApplicationRunner { self =>

  var program: Int => Int = { x => x } // crashes if we refer to myprog directly!! GRRR ...
  override def main(): Unit = {
    val (arg,block) = reify0[Int,Int](program)
    // discard arg; hacked it to be a const ...
    reflect[Unit](block) // ok??
  }
  // mix in delite and lancet generators
  val scalaGen = new ScalaCodegen { val IR: self.type = self }//; Console.println("SCG"); allClass(this.getClass) }
  override def createCodegen() = new ScalaCodegen { val IR: self.type = self }
  override def getCodeGenPkg(t: Target{val IR: self.type}) : 
    GenericFatCodegen{val IR: self.type} = t match {
      case _:TargetScala => createCodegen()
      case _ => super.getCodeGenPkg(t)
    }
  override val deliteGenerator = new DeliteCodegen { 
    val IR : self.type = self;
    val generators = self.generators; 
    //Console.println("DCG")
    //allClass(this.getClass)
  }
  
}

/*def allClass(x: Class[_]): Unit = {
  def rec(x: Class[_]): List[Class[_]] = {
  if (x ne null) {
    x::x.getInterfaces.flatMap(rec).toList:::{
    if (x.getSuperclass ne x) rec(x.getSuperclass) else Nil}
  }.distinct.sortBy(_.toString) else Nil }
  rec(x).foreach(Console.println)
}*/



// *** code generator overrides

trait DeliteCodegen extends DeliteCodeGenPkg { self =>
  val IR: DeliteApplication with Core_LMS
  import IR._
  override def runTransformations[A:Manifest](b: Block[A]): Block[A] = {
    //Console.println("no transformations on block "+b)
    b
  }

  /*override def getExactScope[A](currentScope: List[Stm])(result: List[Exp[Any]]): List[Stm] = {
    val level = super.getExactScope(currentScope)(result)
    //println("delite current scope for "+result)
    //currentScope foreach println
    println("delite level scope for "+result)
    level foreach println
    level
  }*/
}

trait ScalaCodegen extends OptiMLCodeGenScala with GEN_Scala_LMS { 
  val IR: DeliteApplication with OptiMLExp with Core_LMS //LancetImpl
  import IR._

  /*override def getExactScope[A](currentScope: List[Stm])(result: List[Exp[Any]]): List[Stm] = {
    val level = super.getExactScope(currentScope)(result)
    //Console.println("scala current scope for "+result)
    //currentScope.foreach(Console.println)
    Console.println("scala level scope for "+result)
    level.foreach(Console.println)
    level
  }*/

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
    if (syms.length.equals(1) && syms.head.tp.equals(manifest[Unit]))
      stream.println("}}")
    else super.emitKernelFooter(syms,vals,vars,resultType,resultIsVar,external)
  }
  override def emitFileHeader() {
    super.emitFileHeader()
    stream.println("import generated.scala.LancetUtils._")
  }
  /*override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
    super.emitKernelHeader(syms,vals,vars,resultType,resultIsVar,external)
    stream.println("import generated.scala.LancetUtils._")
  }*/
  override def emitDataStructures(path: String) {
    super.emitDataStructures(path)
    val s = java.io.File.separator
    val outFile = path + s + "LancetUtils.scala"
    val out = new PrintWriter(new FileWriter(outFile))
    out.println("package generated.scala")
    out.println("import sun.misc.Unsafe")
    out.println("object LancetUtils {")
    out.println("val unsafe = { val fld = classOf[Unsafe].getDeclaredField(\"theUnsafe\"); fld.setAccessible(true); fld.get(classOf[Unsafe]).asInstanceOf[Unsafe]; }")
    out.println("var RES: Any = _")

    out.println("/*" + IR.asInstanceOf[LancetDeliteRunner].VConstantPool + "*/")
    out.println("/*" + IR.asInstanceOf[LancetDeliteRunner].staticDataMap + "*/")

    for ((s,v) <- IR.asInstanceOf[LancetDeliteRunner].VConstantPool) { // right time?
      val s1 = s.asInstanceOf[Exp[Any]]
      out.println("def p"+quote(s1)+": "+remap(s1.tp)+" = ppl.delite.runtime.graph.ops.Arguments.staticData(\""+s1.toString+"\")")
    }

    out.println("}")
    out.close()

  }

}


// *** delite-specific lancet support

trait LancetImpl extends BytecodeInterpreter_LMS_Opt { 
  import com.oracle.graal.api.meta._      // ResolvedJavaMethod
  import com.oracle.graal.hotspot._
  import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

  // *** random stuff

  // TODO: DeliteIfThenElse !
  //override def if_[T:TypeRep](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T]
  //  = this.asInstanceOf[VectorOperatorsRunnerC].ifThenElse(x,y,z,false)  // DeliteIfThenElse ...


  override def if_[T:TypeRep](x: Rep[Boolean])(y: =>Rep[T])(z: =>Rep[T]): Rep[T] = eval(x) match {
    case VConst(x) => if (x) y else z
    case _ => 

    //val save = exprs
    // TODO: state lub; reset exprs for both branches!

    val store0 = store
    val yb = reify(y)
    //println("store1")
    //println(store)

    store = store0
    val zb = reify(z)
    //println("store1")
    //println(store)

    //println("IF "+x+"="+Def.unapply(x))
    yb.res match {
      case Def(Reify(c,u,es)) => 
        //println("   "+yb+"="+Def.unapply(yb.res))
        //println("   "+es.map(e=>Def.unapply(e)))
      case _ =>
    }
    zb.res match {
      case Def(Reify(c,u,es)) => 
        //println("   "+zb+"="+Def.unapply(zb.res))
        //println("   "+es.map(e=>Def.unapply(e)))
      case _ =>
    }

    // TODO: LMS effects
    var r = reflect[T](IfElse(x,yb,zb))
    //exprs = save
    r
  }

  handler = execMethodPostDom
  

  // *** macro interface

  abstract class ClassMacros {
    val targets: List[Class[_]]
  }

  var classMacros: List[ClassMacros] = Nil

  val macroTable: mutable.HashMap[String, List[Rep[Object]] => Rep[Object]] = new mutable.HashMap

  def install(m: ClassMacros) = {
    classMacros = m :: classMacros
    val repClass = classOf[Rep[_]]
    for (meth <- m.getClass.getMethods) {
      if (meth.getReturnType == repClass || meth.getParameterTypes.exists(_ == repClass)) {
        assert(meth.getReturnType == repClass && meth.getParameterTypes.forall(_ == repClass)) // TODO: error message

        // TODO: automatically convert to constants if parameter is non-rep
        // TODO: 

        // find matching method in class
        var hits = 0
        for (cl <- m.targets; origMeth <- cl.getMethods) {
          // static method: num of parameter types??
          if (origMeth.getName == meth.getName && origMeth.getParameterTypes.length + 1 == meth.getParameterTypes.length) {
            assert(hits == 0, "multiple target methods found for macro method '"+meth.getName+"'")
            val fullName = cl.getName + "." + origMeth.getName
            macroTable(fullName) = { xs: List[Rep[Object]] => meth.invoke(m,xs:_*).asInstanceOf[Rep[Object]] }
            hits += 1
          }
        }
        assert(hits != 0, "no target method found for macro method '"+meth.getName+"'")
      }
    }
  }

  // *** macro implementations
  
  // basically a clone of compile{} that doesn't compile...
  def reify0[A:Manifest,B:Manifest](f: A=>B): (Rep[A],Block[B]) = {

    implicit val tp = manifestToTypeRep(manifest[B])
    val (maStr, mbStr) = (manifestStr(manifest[A]), manifestStr(manifest[B]))

    val arg = liftConst[Object](7:Integer) // just some dummy...
    val y = reify {

      //emitString("import sun.misc.Unsafe")
      //emitString("import generated.scala.DenseVectorDouble")
      //emitString("val unsafe = { val fld = classOf[Unsafe].getDeclaredField(\"theUnsafe\"); fld.setAccessible(true); fld.get(classOf[Unsafe]).asInstanceOf[Unsafe]; }")
      //emitString("type char = Char")
      //emitString("def WARN = assert(false, \"WARN\")")
      //emitString("def ERROR = assert(false, \"ERROR\")")

      //emitString("  var RES = null.asInstanceOf["+mbStr+"]")


      execute(f.getClass.getMethod("apply", manifest[A].erasure), Array[Rep[Object]](unit(f),arg.asInstanceOf[Rep[Object]])(repManifest[Object]))        

      DynExp[B]("RES")
    }
    (arg.asInstanceOf[Rep[A]],y)
  }

  def decompileInternal[A:TypeRep,B:TypeRep](f: Rep[Object]): (Rep[Object],Block[Object]) = {
    val arg = fresh[Object] // A?
    val body = reify {
      val Partial(fs) = eval(f)
      val Static(cls: Class[_]) = fs("clazz")
      withScope {
        //println("{ object BODY {")
        emitString("  var RES = null.asInstanceOf["+typeRep[B]+"]")
        execute(cls.getMethod("apply", Class.forName("java.lang.Object")), Array[Rep[Object]](f,arg)(repManifest[Object]))
        //println("}")
        //"BODY.RES.asInstanceOf["+typeRep[B]+"]}"
        Dyn[Object]("RES.asInstanceOf["+typeRep[B]+"]")
      }
    }
    (arg,body)
  }

  var traceMethods = false

  def handleMethodCall(parent: InterpreterFrame, m: ResolvedJavaMethod): Option[InterpreterFrame] = {
    val className = m.getDeclaringClass.toJava.getName
    val fullName = className + "." + m.getName
    var continuation: InterpreterFrame = parent
    def handle(f: List[Rep[Object]] => Rep[Object]): Option[InterpreterFrame] = {
      val returnValue = f(popArgumentsAsObject(parent, m, !java.lang.reflect.Modifier.isStatic(m.getModifiers)).toList)

      //println("self return kind: "+m.getSignature().getReturnKind())
      //println("cont return kind: "+continuation.getMethod.getSignature().getReturnKind())

      //pushAsObject(continuation, continuation.getMethod.getSignature().getReturnKind(), returnValue)
      pushAsObject(continuation, m.getSignature().getReturnKind(), returnValue)
      Some(if (continuation == parent) null else continuation)
    }

    if (traceMethods) Console.println("// "+fullName)

    // check for known methods
    if (macroTable.contains(fullName)) {
      handle(macroTable(fullName))
    } else fullName match {

      case "scala.runtime.BoxesRunTime.boxToBoolean" => handle {
        case r::Nil => reflect[java.lang.Boolean](r,".asInstanceOf[java.lang.Boolean]")(mtr[java.lang.Boolean])
      }
      case "scala.runtime.BoxesRunTime.unboxToBoolean" => handle {
        case r::Nil => reflect[Boolean](r,".asInstanceOf[Boolean]").asInstanceOf[Rep[Object]]
      }
      case "scala.runtime.BoxesRunTime.boxToInteger" => handle {
        case r::Nil => reflect[java.lang.Integer](r,".asInstanceOf[java.lang.Integer]")(mtr[java.lang.Integer])
      }
      case "scala.runtime.BoxesRunTime.unboxToInt" => handle {
        case r::Nil => reflect[Int](r,".asInstanceOf[Int]").asInstanceOf[Rep[Object]]
      }
/*
      java.lang.Boolean.valueOf ?
      java.lang.Integer.valueOf ?
*/          
      case "scala.Predef$.println" => handle {
        case self::r::Nil => reflect[Unit]("println(",r,")").asInstanceOf[Rep[Object]]
      }
      case _ => 
        //println(fullName)
        None
    }
  }


  override def isSafeRead(base: Object, offset: Long, field: ResolvedJavaField, typ: TypeRep[_]): Boolean =
    super.isSafeRead(base, offset, field, typ) || {
      val name = field.getDeclaringClass.toJava.getName + "." + field.getName
      name match {
        case _ =>
         false
      }
    }


  override def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame =
    handleMethodCall(parent,m).getOrElse(super.resolveAndInvoke(parent, m))

  override def invokeDirect(parent: InterpreterFrame, m: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame =
    handleMethodCall(parent,m).getOrElse(super.invokeDirect(parent, m, hasReceiver))

}



// *** from delite test runner. call compileAndTest to run an app

object DeliteRunner {

  val MAGICDELIMETER = "!~x02$758209"

  val propFile = new File("delite.properties")
  if (!propFile.exists) throw new TestFailedException("could not find delite.properties", 3)
  val props = new java.util.Properties()
  props.load(new FileReader(propFile))

  // test parameters
  val verbose = props.getProperty("tests.verbose", "false").toBoolean
  val verboseDefs = props.getProperty("tests.verboseDefs", "false").toBoolean
  val threads = props.getProperty("tests.threads", "1")
  val cacheSyms = false /* NNOOOOOOOOOO!!!!!!!!!!!*/   //props.getProperty("tests.cacheSyms", "true").toBoolean
  val javaHome = new File(props.getProperty("java.home", ""))
  val scalaHome = new File(props.getProperty("scala.vanilla.home", ""))
  val runtimeClasses = new File(props.getProperty("runtime.classes", ""))
  val runtimeExternalProc = false // scalaHome and runtimeClasses only required if runtimeExternalProc is true. should this be configurable? or should we just remove execTestExternal?

  val javaBin = new File(javaHome, "bin/java")
  val scalaCompiler = new File(scalaHome, "lib/scala-compiler.jar")
  val scalaLibrary = new File(scalaHome, "lib/scala-library.jar")

  def validateParameters() {
    if (!javaHome.exists) throw new TestFailedException("java.home must be a valid path in delite.properties", 3)
    else if (!javaBin.exists) throw new TestFailedException("Could not find valid java installation in " + javaHome, 3)
    else if (runtimeExternalProc && !scalaHome.exists) throw new TestFailedException("scala.vanilla.home must be a valid path in delite.proeprties", 3)
    else if (runtimeExternalProc && (!scalaCompiler.exists || !scalaLibrary.exists)) throw new TestFailedException("Could not find valid scala installation in " + scalaHome, 3)
    else if (runtimeExternalProc && !runtimeClasses.exists) throw new TestFailedException("runtime.classes must be a valid path in delite.properties", 3)
  }


  def compileAndTest(app: DeliteTestRunner) {
    compileAndTest2(app, app.getClass.getName.replaceAll("\\$", ""))
  }

  def compileAndTest2(app: DeliteTestRunner, uniqueTestName: String) {
    println("=================================================================================================")
    println("TEST: " + app.toString)
    println("=================================================================================================")

    validateParameters()
    val args = Array(uniqueTestName + "-test.deg")
    app.resultBuffer = new ArrayBuffer[Boolean] with SynchronizedBuffer[Boolean]
    stageTest(app, args(0), uniqueTestName)
    val outStr = execTest(app, args, uniqueTestName) // if (runtimeExternalProc..)?
    checkTest(app, outStr)
  }

  def stageTest(app: DeliteTestRunner, degName: String, uniqueTestName: String) = {
    println("STAGING...")
    val save = Config.degFilename
    val buildDir = Config.buildDir
    val saveCacheSyms = Config.cacheSyms
    val generatedDir = (new File("generated")).getAbsolutePath + /*protobuf wants absolute path*/
      java.io.File.separator + uniqueTestName
    try {
      Config.degFilename = degName
      Config.buildDir = generatedDir
      Config.cacheSyms = cacheSyms
      val screenOrVoid = if (verbose) System.out else new PrintStream(new ByteArrayOutputStream())
      Console.withOut(screenOrVoid) {
        app.main(Array())
        if (verboseDefs) app.globalDefs.foreach { d => //TR print all defs
          println(d)
          //val s = d match { case app.TP(sym,_) => sym; case app.TTP(syms,_,_) => syms(0); case _ => sys.error("unknown Stm type: " + d) }
          //val info = s.sourceInfo.drop(3).takeWhile(_.getMethodName != "main")
          //println(info.map(s => s.getFileName + ":" + s.getLineNumber).distinct.mkString(","))
        }
        //assert(!app.hadErrors) //TR should enable this check at some time ...
      }
    } finally { 
      // concurrent access check 
      assert(Config.buildDir == generatedDir)
      Config.degFilename = save
      Config.buildDir = buildDir
      Config.cacheSyms = saveCacheSyms
    }
  }

  def execTest(app: DeliteTestRunner, args: Array[String], uniqueTestName: String) = {
    println("EXECUTING...")
    ppl.delite.runtime.profiler.PerformanceTimer.times.clear // don't print running time (messes up check file)
    val name = "test.tmp"
    System.setProperty("delite.threads", threads.toString)
    System.setProperty("delite.code.cache.home", "generatedCache" + java.io.File.separator + uniqueTestName)
    //Console.withOut(new PrintStream(new FileOutputStream(name))) {
      println("test output for: " + app.toString)
      // NOTE: DeliteCodegen (which computes app.staticDataMap) does not know about VConstantPool!!!
      val staticDataMap = app match {
        case app: Base_LMS => app.VConstantPool.map(kv=>kv._1.toString->kv._2).toMap
        case app => app.staticDataMap
      }
      ppl.delite.runtime.Delite.embeddedMain(args, staticDataMap) // was: app.staticDataMap
    //}
    /*val buf = new Array[Byte](new File(name).length().toInt)
    val fis = new FileInputStream(name)
    fis.read(buf)
    fis.close()
    val r = new String(buf)
    if (verbose) System.out.println(r)
    r*/""
  }


  def checkTest(app: DeliteTestRunner, outStr: String) {
    println("CHECKING...")
    /*val resultStr = outStr substring (outStr.indexOf(MAGICDELIMETER) + MAGICDELIMETER.length, outStr.lastIndexOf(MAGICDELIMETER))
    val results = resultStr split ","
    for (i <- 0 until results.length) {
      if (verbose) print("  condition " + i + ": ")
      val passed = results(i).toLowerCase() == "true"
      if (verbose)
        if (passed) println("PASSED") else println("FAILED")
      assert(passed)
    }*/
  }
}


class TestFailedException(s: String, i: Int) extends Exception(s)

trait DeliteTestRunner extends DeliteTestModule with DeliteApplication
  with MiscOpsExp with SynchronizedArrayBufferOpsExp with StringOpsExp {

  var resultBuffer: ArrayBuffer[Boolean] = _

  def collector: Rep[ArrayBuffer[Boolean]] = staticData(resultBuffer)
}

trait DeliteTestModule extends Object
  with MiscOps with SynchronizedArrayBufferOps with StringOps {

  def main(): Unit

  def collector: Rep[ArrayBuffer[Boolean]]

  def collect(s: Rep[Boolean]) { collector += s; println(s) }

  def mkReport(): Rep[Unit] = {
    println(unit(DeliteRunner.MAGICDELIMETER) + (collector mkString unit(",")) + unit(DeliteRunner.MAGICDELIMETER))
  }
}
