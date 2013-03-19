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

    val MAGICDELIMETER = "!~x02$758209"

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

  class TestFailedException(s: String, i: Int) extends Exception(s)

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

  compileAndTest(VectorOperatorsRunner)



/*

    val it = newCompiler
    val f = it.compile { (x:Int) => 
        val b = new Bar
        b.intField = 7
        if (x > 0) {
          b.intField = 9
        }
        b.intField
    }
    printcheck(f(7), 9)
*/


  }


}