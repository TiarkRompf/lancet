/*
 * Copyright (c) %YEARS% Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/agpl.html.
 * 
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package lancet.interpreter

import java.io._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader


object ScalaCompile {

  var compiler: Global = _
  var external = false

  def setCompiler(c: Global): Unit = {
    compiler = c
    external = true
  }

  def setupCompiler() = {
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
    settings.deprecation.value = true
    //settings.verbose.value = true
    // -usejavacp needed on windows?

    val reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }

  // mostly to generate deterministic test output
  def reset() = {
    compileCount = 0
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

    if (external) {    
      run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))
      val loader = Thread.currentThread().getContextClassLoader
      val cls: Class[_] = loader.loadClass(className)
      val cons = cls.getConstructor(staticData.map(_._2):_*)
      cons.newInstance(staticData.map(_._1):_*).asInstanceOf[A=>B]
    } else {
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
}