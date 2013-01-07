package playground.interpreter

import java.io._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader


object ScalaCompile {

  var compiler: Global = _
  //var output: ByteArrayOutputStream = _ 

  def setupCompiler() = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
    */
    val settings = new Settings()

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    //settings.verbose.value = true
    // -usejavacp needed on windows?

    val reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }

  var compileCount = 0
  
  var dumpGeneratedCode = false

  def compile[A:Manifest,B:Manifest](source: String, className: String, staticData: List[(AnyRef,Class[_])]): A=>B = {
    if (this.compiler eq null)
      setupCompiler()
    
    compileCount += 1
    
    if (dumpGeneratedCode) println(source)

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
  //      compiler.genJVM.outputDir = fileSystem

    val reporter = new ConsoleReporter(compiler.settings, null, new PrintWriter(System.out)) // create new reporter because System.out may have changed
    compiler.reporter = reporter

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))

    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")

    reporter.reset
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor(staticData.map(_._2):_*)
    
    val obj: A=>B = cons.newInstance(staticData.map(_._1):_*).asInstanceOf[A=>B]
    obj
  }
}