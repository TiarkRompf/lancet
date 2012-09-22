Experiments with the Graal VM.

### Background:

- [Graal VM](http://openjdk.java.net/projects/graal/)


### How to run:

1. Install the simple build tool ([SBT](http://www.scala-sbt.org/)), version 0.12. Follow the installation instructions on the [SBT website](http://www.scala-sbt.org/download.html).

2. Make sure the sbt launcher script honors the `JAVA_HOME` and `JAVA_OPTS` environment variables.

3. Download and build the Graal VM anywhere on your file system. Follow the instructions on the [Graal website](http://openjdk.java.net/projects/graal/). 

4. Set `JAVA_HOME` and `JAVA_OPTS` environment variables.

        export JAVA_HOME="~/graal/jdk1.7.0/product/" # Graal build from step 3
        export JAVA_OPTS="-graal"

   Alternatively, modify the `gsbt` file to reflect your paths and use that instead of the system-wide `sbt`.

5. Run `sbt` to start the sbt console, then `test` to run the test suite.
