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