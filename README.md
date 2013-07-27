Project Lancet: Surgical Precision JIT Compilers
================================================

Just-in-time compilation faces more optimization opportunities than offline
compilation. Common JIT compilers rely on a limited set of dynamic properties
(obtained e.g. by profiling) to guide optimizations. Unfortunately, their
behavior is a black box, and in many cases, the achieved performance is highly
unpredictable.

The goal of Project Lancet is to turn JIT compilation into a precision tool.

Leveraging recent results on staging, macros and partial evaluation, the aim
of this project is to develop a JIT compiler framework that is tightly
integrated with the running program itself. This will enable more
deterministic performance and easy extensibility -- for example allowing
library developers to supply domain-specific compiler optimizations.

A draft paper with more information and experimental results
is available here: 

- [Project Lancet: Surgical Precision JIT Compilers](http://lamp.epfl.ch/~rompf/lancet_130329.pdf)


Examples
--------

Below are short examples of some of the planned functionality.

### Explicit Compilation

JIT compilation of parts of the running program can be invoked manually. A
common use case is to compile multiple specialized versions of a generic
function. In the snippet below, method `foo` is partially applied to the value
6, and `compile` ensure that the result `foo6` is specialized to that value:

```scala
  def foo(x: Int, y: Double) = ...
  val foo6 = compile(y => foo(6, y))
  foo6(7) // same as foo(6,7), but faster
```


### Partial Evaluation / Symbolic Execution

To enable successful specialization, JIT compilers need to be able to perform
elaborate symbolic execution. However, fully automatic partial evaluation is
unrealistic for large programs; user annotations in key places are needed and
desirable to communicate intent.

There are quite a few different ways these annotations can be realized. A
simple yet powerful way is to introduce a special marker method `frozen`,
which asserts that its argument be a compile-time constant:

```scala
  val x = ...
  val y = frozen(x) // fails if x is not a compile time constant
```

If the argument to `frozen` is not a constant, JIT compilation will fail with
an error message.

In addition to the statement level, one can also put corresponding annotations
on method parameters:

```scala
  def foo(@frozen x: Int, y: Double) = ...
  foo(...) // fails if argument is not known at compile time
```

The key benefit of frozen values is that programs can be reliably and
deterministically simplified at (JIT) compile time:

```scala
  if (frozen(cond)) { ... } else { ... }
```

In this case, we obtain a guarantee that if JIT compilation succeeds, the
conditional will be eliminated because the condition is static.

### Speculative Optimizations

In addition to straight program specialization, we want to put the dynamic VM
capabilities to good use, namely profiling, speculative optimization, and the
facility to discard optimized code if it later turns out that compilation
assumptions were too optimistic (deoptimization).

To start with a simple example, a method `likely` may tell the compiler to
assume that a test will likely succeed:

```scala
  if (likely(cond)) { ... } else { ... }
```

In this case, the VM could come to the same conclusion by profiling the actual
execution and recording the branches taken. However, profiling may not be
accurate if it is done independently of the calling context, and even if it is,
there is considerable utility in doing both: The use of `likely` can serve as
a contract, and cause the VM to signal a warning if profiling suggests that
the test is actually not likely.

To give the compiler even tougher guidelines, we introduce a method
`speculate`:

```scala
  if (speculate(cond)) { ... } else { ... }
```

This will instruct the compiler to assume that the test will always succeed
and optimize the method accordingly, replacing the conditional by its then
branch. But of course the compiler cannot be certain that this assumption
always holds so it must insert a guard that, if the check fails, takes a slow
path and drops into interpreter mode for this particular execution.

A variant of this approach is `stable`:

```scala
  if (stable(cond)) { ... } else { ... }
```

The assumption here is that the condition may change permanently but
infrequently. If the guard fails, the method is recompiled on the fly with the
new value.

This concept of stable state applies not only to conditionals but can be
generalized to arbitrary variables (the Graal/Truffle framework has something
similar). Adding a `@stable` annotation to any variable will read the value at
compile time and treat it as constant for the compilation

```scala
  class Foo {
    @stable var v = ...
  }
  val foo: Foo = ...
  if (foo.v == 8) { ... }   // static test, conditional eliminated
  val w = frozen(foo.v)     // will never fail
```

The necessary guard checks will trigger recompilation if a change is detected.


### Explicit Staging

In addition to more implicit symbolic execution guided by marker methods, we
can also employ explicit staging constructs. Below, we use a Lisp-style
`quote` and `unquote` facility to explicitly combine code fragments:

```scala
  def power(b: Code[Int], x: Int): Code[Int] =  { // use `unquote`
    println("static: power " + x)
    if (x == 0) quote(1) else quote(unquote(b) * unquote(power(b, x-1)))
  }
  println("--- static")
  val pow4 = compile { b: Code[Int] => power(b, 4) }
  println("--- dynamic")
  val y = pow4(2)
```

The key difference to partial evaluation as outlines above is that we can
perform arbitrary computations at staging time (e.g. store pieces of code in a
data structure), whereas partial evaluation always preserves the semantics and
execution order of the program. In the example shown, the implementation of
`power` prints a message to stdout, during compilation. This is achieved by
`unquote` instantiating its argument closure at compile time and issuing a
reflective call. Note that different types are used for staged and
compile-times values (`Code[T]`).


### JIT Macros

The ability to perform arbitrary computation at compile time can be put to use
to implement a general macro facility, where given method calls are redirected
to user-define code, which is invoked at compile-time to "fill in the blanks",
i.e. compute a program fragment to be used in place of the original method
call.

The primary use case for JIT macros are domain specific optimizations. A
developer of a linear algebra library, for example, may want to provide a set
of macros that optimize algebraic operations on vectors and matrices:

```scala
  abstract class Matrix[T] {
    def *(m: Matrix[T]): Matrix[T]
  }
  object MatrixFactory {
    def id(n: Int): Matrix[T] = ...
  }
  object MatrixMacros extends ClassMacros {
    val adaptedClasses = List(classOf[Matrix[_]], classOf[MatrixFactory])
    def *(x: Code[Matrix[T]], y: Code[Matrix[T]]): Code[Matrix[T]] = {
      // compile-time computation:
      //   inspect x,y, if one of them is id(..) just return the other
      //   otherwise return IR fragment that implements a BLAS call
    }
  }
  Compiler.install(MatrixMacros)
  Compiler.compile {
    val id = MatrixFactory.id(20)
    val m = ...
    val m = m * id
  }
```


### Cross-Compiling to External Targets

Given the ability to reify pieces of the running program into `Code[T]`
objects by decompiling byte code, it is possible to cross-compile program
fragments to external targets (SQL, JavaScript, CUDA, C, VHDL, ...).




Logistics
---------

Project Lancet currently requires the [Graal
VM](http://openjdk.java.net/projects/graal/). Future versions may also provide
some of the functionality on off-the-shelf HotSpot VMs.


### How to build and run

1. Install the simple build tool ([SBT](http://www.scala-sbt.org/)), version
0.12. Follow the installation instructions on the [SBT
website](http://www.scala-sbt.org/download.html).

2. Make sure the sbt launcher script honors the `JAVA_HOME` and `JAVA_OPTS`
environment variables.

3. Download and build the Graal VM anywhere on your file system. Follow the
instructions on the [Graal website](http://openjdk.java.net/projects/graal/).
Get commit `7839:838293a77af7`.

4. Set `JAVA_HOME` and `JAVA_OPTS` environment variables.

```
  export JAVA_HOME="~/graal/jdk1.7.0/product/" # Graal build from step 3
  export JAVA_OPTS="-graal -Ddelite.home.dir=~/delite"  # Delite home
```

  Alternatively, modify the `gsbt` file to reflect your paths and use that
  instead of the system-wide `sbt`.

5. Install dependencies: LMS-Core and Delite
    - [LMS-Core](http://github.com/tiarkrompf/virtualization-lms-core): 
      branch `wip-delite-develop` (latest commit tested: 2190c9ad07cdf5aa63b138a73440050291b82699). 
      Edit the `build.sbt` file and remove the `scalaBinaryVersion` key.
      Run `sbt publish-local` inside your local clone dir.
    - [Delite](http://github.com/stanford-ppl/delite): 
      branch `wip-clusters-lancet` (latest commit tested: 9c0599dd60614811e48ad5d15e9a696bb9a3d76e).
      Run `sbt publish-local` and `sbt delite-test/publish-local` inside your local clone dir. There might
      be compile errors but that is OK.
    - Create a `delite.properties` file in your local lancet clone 
      (contents as described [here](http://github.com/stanford-ppl/delite)).

6. Run `sbt` to start the sbt console, then `test` to run the test suite.


### License

For the time being, Project Lancet is licensed under the [AGPLv3](http://www.gnu.org/licenses/agpl.html). More
permissive licensing may be available in the future.

### Disclaimers

One or more authors are employees of Oracle Labs.
The views expressed here are their own and do not necessarily reflect the views of Oracle. 
