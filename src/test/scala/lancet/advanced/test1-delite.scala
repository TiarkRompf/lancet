package lancet
package advanced

import lancet.api._
import lancet.interpreter._

import ppl.dsl.optiml.{Vector,DenseVector,RangeVector,IndexVectorRange}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config
import scala.virtualization.lms.common._
import scala.collection.mutable.{ ArrayBuffer, SynchronizedBuffer }
import java.io.{ File, Console => _, _ }
import java.io.FileSystem


class TestDelite1 extends FileDiffSuite {
  
  val prefix = "test-out/test-delite-1"

  def newCompiler = new BytecodeInterpreter_LMS_Opt {
    initialize()
    debugBlockKeys = false
  }

  def testA1 = withOutFileChecked(prefix+"A1") {
    import DeliteRunner._

    object VectorOperatorsRunner extends DeliteTestRunner with OptiMLApplicationRunner with VectorOperators
    trait VectorOperators extends DeliteTestModule with OptiMLApplication {
      def main() {
        val v = Vector.rand(1000)

        val vt = v.t
        collect(vt.isRow != v.isRow)

        //val vc = v.clone
        //collect(vc.cmp(v) == true)

        val v2 = Vector(1,2,3,4,5)
        //collect(median(v2) == 3)
        collect(mean(v2) == 3)
        collect(max(v2) == 5)
        collect(min(v2) == 1)
        collect(mean(3,6,2,5) == 4.0)
        
        mkReport
      }
    }
  
    compileAndTest(VectorOperatorsRunner)
  }


  def testA2 = withOutFileChecked(prefix+"A2") {
    import DeliteRunner._

    /* TODO: 
      - conflicts: IfThenElse, compiler {}
      - make delite scala codegen generate lancet stuff
      - lancet decompile bytecode -> delite backend
      - ISSUE: control flow?? need to use DeliteIf, and don't have functions ...
    */

    object Vector {
      def rand[T](n:Int): Vector[T] = { println("Vector$.rand"); new Vector[T] }
      def apply[T](xs: T*): Vector[T] = { println("Vector$.apply"); new Vector[T] }
    }
    class Vector[T] {
      def t: Vector[T] = { println("Vector.t"); new Vector[T] }
      def isRow: Boolean = { println("Vector.isRow"); false }
    }

    object Util {
      def mean(v: Vector[Int]): Int = { println("Util.mean"); 0} 
      def max(v: Vector[Int]): Int = { println("Util.mean"); 0 }
      def min(v: Vector[Int]): Int = { println("Util.mean"); 0 }
      def collect(b: Boolean): Unit = { println("Util.mean") }
    }

    def myprog = {
      import Util._

      val v = Vector.rand(1000)

      val vt = v.t
      collect(vt.isRow != v.isRow)

      //val vc = v.clone
      //collect(vc.cmp(v) == true)

      val v2 = Vector(1,2,3,4,5)
      //collect(median(v2) == 3)
      collect(mean(v2) == 3)
      collect(max(v2) == 5)
      collect(min(v2) == 1)
    }



    object Macros {
      import VectorOperatorsRunner._

      def Vector_rand(n: Rep[Int]): Rep[Vector[Int]] = 
        reflect[Vector[Int]]("VectorRand(",n,")")(mtr[Vector[Int]])
      def Vector_apply[T](xs: Rep[Seq[T]]): Rep[Vector[T]] = 
        reflect[Vector[T]]("VectorApply(",xs,")")(mtr[Vector[Int]].relax)
        // TODO: generic types are still problematic...

    }


    object VectorOperatorsRunner extends LancetImpl
      with DeliteTestRunner with OptiMLApplicationRunner {
        //override def compile[A:Manifest,B:Manifest](f: A => B): A=>B = ??? {
          def main() = ()//myprog

    }


    trait LancetImpl extends BytecodeInterpreter_LMS_Opt { 
      import com.oracle.graal.api.meta._      // ResolvedJavaMethod
      import com.oracle.graal.hotspot._
      import com.oracle.graal.hotspot.meta._  // HotSpotRuntime

      // *** macro implementations
      
      def decompileInternal[A:TypeRep,B:TypeRep](f: Rep[Object]): (Rep[Object],Block[Object]) = {
        val arg = fresh[Object] // A?
        val body = reify {
          val Partial(fs) = eval(f)
          val Static(cls: Class[_]) = fs("clazz")
          withScope {
            //println("{ object BODY {")
            emitString("  var RES = null.asInstanceOf["+typeRep[B]+"]")
            execute(cls.getMethod("apply", classOf[Object]), Array[Rep[Object]](f,arg)(repManifest[Object]))
            //println("}")
            //"BODY.RES.asInstanceOf["+typeRep[B]+"]}"
            Dyn[Object]("RES.asInstanceOf["+typeRep[B]+"]")
          }
        }
        (arg,body)
      }

      var traceMethods = true

      def handleMethodCall(parent: InterpreterFrame, m: ResolvedJavaMethod): Option[InterpreterFrame] = {
        val className = m.getDeclaringClass.toJava.getName
        val fullName = className + "." + m.getName
        var continuation: InterpreterFrame = parent
        def handle(f: List[Rep[Object]] => Rep[Object]): Option[InterpreterFrame] = {
          val returnValue = f(popArgumentsAsObject(parent, m, !java.lang.reflect.Modifier.isStatic(m.getModifiers)).toList)
          pushAsObject(continuation, continuation.getMethod.getSignature().getReturnKind(), returnValue)
          Some(if (continuation == parent) null else continuation)
        }

        if (traceMethods) Console.println("// "+fullName)

        val vector_rand = Vector.getClass.getName+".rand"
        val vector_apply = Vector.getClass.getName+".apply"

        type R[T] = VectorOperatorsRunner.Rep[T]

        // check for known methods
        fullName match {

          case `vector_rand` => handle {
            case r::n::Nil => Macros.Vector_rand(n.asInstanceOf[R[Int]]).asInstanceOf[Rep[Object]]
          }

          case "scala.runtime.BoxesRunTime.boxToInteger" => handle {
            case r::Nil => reflect[Integer](r,".asInstanceOf[Integer]")(mtr[Integer])
          }
          case "scala.runtime.BoxesRunTime.unboxToInt" => handle {
            case r::Nil => reflect[Int](r,".asInstanceOf[Int]").asInstanceOf[Rep[Object]]
          }
          case "scala.Predef$.println" => handle {
            case r::Nil => reflect[Object]("println(",r,")")
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

    VectorOperatorsRunner.initialize()
    VectorOperatorsRunner.traceMethods = true
    VectorOperatorsRunner.compile0((x: Int) => myprog)
  }
}




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
  val cacheSyms = props.getProperty("tests.cacheSyms", "true").toBoolean
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
          val s = d match { case app.TP(sym,_) => sym; case app.TTP(syms,_,_) => syms(0); case _ => sys.error("unknown Stm type: " + d) }
          val info = s.sourceInfo.drop(3).takeWhile(_.getMethodName != "main")
          println(info.map(s => s.getFileName + ":" + s.getLineNumber).distinct.mkString(","))
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
    val name = "test.tmp"
    System.setProperty("delite.threads", threads.toString)
    System.setProperty("delite.code.cache.home", "generatedCache" + java.io.File.separator + uniqueTestName)
    Console.withOut(new PrintStream(new FileOutputStream(name))) {
      println("test output for: " + app.toString)
      ppl.delite.runtime.Delite.embeddedMain(args, app.staticDataMap)
    }
    val buf = new Array[Byte](new File(name).length().toInt)
    val fis = new FileInputStream(name)
    fis.read(buf)
    fis.close()
    val r = new String(buf)
    if (verbose) System.out.println(r)
    r
  }


  def checkTest(app: DeliteTestRunner, outStr: String) {
    println("CHECKING...")
    val resultStr = outStr substring (outStr.indexOf(MAGICDELIMETER) + MAGICDELIMETER.length, outStr.lastIndexOf(MAGICDELIMETER))
    val results = resultStr split ","
    for (i <- 0 until results.length) {
      if (verbose) print("  condition " + i + ": ")
      val passed = results(i).toLowerCase() == "true"
      if (verbose)
        if (passed) println("PASSED") else println("FAILED")
      assert(passed)
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

    def collect(s: Rep[Boolean]) { collector += s }

    def mkReport(): Rep[Unit] = {
      println(unit(MAGICDELIMETER) + (collector mkString unit(",")) + unit(MAGICDELIMETER))
    }
  }


}


