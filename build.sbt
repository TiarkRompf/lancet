name := "lancet"

version := "0.2"

scalaVersion := "2.10.0"

scalaBinaryVersion := "2.10.0"

scalaOrganization := "org.scala-lang.virtualized"

// tests are not thread safe
parallelExecution in Test := false

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.0"

//libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

unmanagedClasspath in Compile <++= baseDirectory map { base =>
  val graal = new File(System.getenv("JAVA_HOME")) / ".." / ".." / "graal"
  Seq(
    "com.oracle.graal.alloc",
    "com.oracle.graal.api",
    "com.oracle.graal.api.code",
    "com.oracle.graal.api.interpreter",
    "com.oracle.graal.api.meta",
    "com.oracle.graal.api.test",
    "com.oracle.graal.boot",
    "com.oracle.graal.boot.test",
    "com.oracle.graal.bytecode",
    "com.oracle.graal.compiler",
    "com.oracle.graal.debug",
    "com.oracle.graal.examples",
    "com.oracle.graal.graph",
    "com.oracle.graal.graph.test",
    "com.oracle.graal.hotspot",
    "com.oracle.graal.hotspot.server",
    "com.oracle.graal.interpreter",
    "com.oracle.graal.java",
    "com.oracle.graal.jtt",
    "com.oracle.graal.lir",
    "com.oracle.graal.lir.amd64",
    "com.oracle.graal.nodes",
    "com.oracle.graal.printer",
    "com.oracle.graal.snippets",
    "com.oracle.graal.snippets.test",
    "com.oracle.graal.tests",
    "com.oracle.max.asm",
    "com.oracle.max.cri",
    "com.oracle.max.criutils",
    "com.oracle.truffle",
    "com.oracle.truffle.bf",
    "com.oracle.truffle.c",
    "com.oracle.truffle.c.serial",
    "com.oracle.truffle.c.test",
    "com.oracle.truffle.compiler",
    "com.oracle.truffle.debugger.model",
    "com.oracle.truffle.debugger.ui",
    "com.oracle.truffle.java",
    "com.oracle.truffle.java.test",
    "com.oracle.truffle.javac",
    "com.oracle.truffle.jlang",
    "com.oracle.truffle.js",
    "com.oracle.truffle.js.test",
    "com.oracle.truffle.jxinterface",
    "com.oracle.truffle.py",
    "com.oracle.truffle.serial",
    "com.oracle.truffle.serial.test"
  ) map (graal / _ / "bin")
}

unmanagedClasspath in Test <++= (unmanagedClasspath in Compile)