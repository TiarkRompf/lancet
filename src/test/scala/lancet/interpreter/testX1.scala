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

package lancet
package interpreter

import lancet.api._

class TestInterpreterX extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-X"

  def test1 = withOutFileChecked(prefix+"repl1") {

    val c = lancet.api.Lancet.newCompilerOpt
    import c._

    println("==== fun/printcode")

{
    def foo(x:Int) = 3 * x + 4

    println(foo(7))

    val f = fun(foo _)

    println(f(7))

    f.printcode
}

{
    def foo(x:Int, y: Int) = 3 * x + y

    val foo6 = fun { y: Int => foo(6,y) }

    foo6.printcode
}


{
    def foo(x:Int, y: Int) = 3 * x + y

    def fooX(x: Int) = fun { y: Int => foo(x,y) }

    val foo6 = fooX(6)

    foo6.printcode
}



    println("==== staging: quote/unquote")

{
    val f = lms { x: Rep[Int] => x * 4 }

    println(f(8))
}

{
    val f = lms { x: Rep[Int] => quote(3 * 4) }

    println(f(8))
}

{
    val f = lms { x: Rep[Int] => quote(3 * unquote(x)) }

    println(f(8))
}




    println("==== continuations: shift/reset")

{
    def foo(x: Int) = shift((k: Int=>Int) => k(x) + k(x))

    try {
      println(foo(7))
    } catch { 
      case e: NotImplementedError =>
      println(e)
    }

    val x = exec { foo(7) + 3 }

    println(x)
}



    println("==== speculate/slowpath")

{
    def speculate(x: Int, y: Int) = shift((k:Int=>Int)=> if (x == y) k(y) else { slowpath; k(x) })

    var x = 10

    val f = fun { speculate(x, 10) + 100 }

    println(f())

    x = 100

    println(f())
}

  }

}
