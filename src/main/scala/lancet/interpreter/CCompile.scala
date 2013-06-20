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

// TODO

object CCompile {

  val csrc = """
#import <jni.h>

JNIEXPORT jint JNICALL Java_Foo_bar (
     JNIEnv *env,        /* interface pointer */
     jobject obj,        /* "this" pointer */
     jint i);             /* argument #1 */



jint Java_Foo_bar (
     JNIEnv *env,        /* interface pointer */
     jobject obj,        /* "this" pointer */
     jint i)             /* argument #1 */
{
     return 2 * i;
}
"""


  val scalasrc = """
class Foo {
  @native def bar(x: Int): Int
}
"""

  def compile[A,B](source: String): A=>B = {

    assert(false, "NOT IMPLEMENTED")


    // write csrc to file
    // ...

    val cmd = "gcc -shared -o /tmp/libtest.dylib -c /tmp/test.c -I /System/Library/Frameworks/JavaVM.framework/Versions/A/Headers"

    // exec cmd

    System.load("/tmp/libtest.dylib")


    // ScalaCompile: compile scalasrc, load class, create object
    // ...

    val obj: { def bar(x: Int): Int } = null


    println(obj bar 7)

    obj.asInstanceOf[A=>B]
  }

}