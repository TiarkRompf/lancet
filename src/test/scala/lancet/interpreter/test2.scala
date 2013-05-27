package lancet
package interpreter

import lancet.api._

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime


class TestInterpreter2 extends FileDiffSuite {

  val prefix = "test-out/test-interpreter-2"

  object Program {

    trait JS

    object Dom extends JS {

      trait Element extends JS 
      trait Canvas extends JS {
        def getContext(key: String): Context
      }
      trait Context extends JS {
        def translate(x: Double, y: Double)
        def rotate(r: Double)
        def moveTo(x: Double, y: Double)
        def lineTo(x: Double, y: Double)
        def scale(x: Double, y: Double)
        def closePath()
        def stroke()
        def restore()
        def save()
      }

      object document extends JS {
        def getElementById(id: String): Element = ???
      }

    }

    import Dom._

    def draw = {
        var canvas = document.getElementById("canvas").asInstanceOf[Canvas];
        var c = canvas.getContext("2d");
        snowflake(c,0,5,115,125);    // A level-0 snowflake is an equilateral triangle
        snowflake(c,1,145,115,125);  // A level-1 snowflake is a 6-sided star
        snowflake(c,2,285,115,125);  // etc.
        snowflake(c,3,425,115,125);
        snowflake(c,4,565,115,125);  // A level-4 snowflake looks like a snowflake!
        c.stroke();                  // Stroke this very complicated path

    }


    var deg = scala.math.Pi/180;  // For converting degrees to radians

    // Draw a level-n Koch Snowflake fractal on the canvas context c,
    // with lower-left corner at (x,y) and side length len.
    @noinline def snowflake(c: Context, n: Int, x: Int, y: Int, len: Int) = {
        c.save();           // Save current transformation
        c.translate(x,y);   // Translate origin to starting point
        c.moveTo(0,0);      // Begin a new subpath at the new origin
        leg(n);             // Draw the first leg of the snowflake
        c.rotate(-120*deg); // Now rotate 120 degrees counterclockwise
        leg(n);             // Draw the second leg
        c.rotate(-120*deg); // Rotate again
        leg(n);             // Draw the final leg
        c.closePath();      // Close the subpath
        c.restore();        // And restore original transformation

        // Draw a single leg of a level-n Koch snowflake.
        // This function leaves the current point at the end of the leg it has
        // drawn and translates the coordinate system so the current point is (0,0).
        // This means you can easily call rotate() after drawing a leg.
        def leg(n: Int) {
            c.save();               // Save the current transformation
            if (n == 0) {           // Nonrecursive case:
                c.lineTo(len, 0);   //   Just draw a horizontal line
            }                       //                                       _  _
            else {                  // Recursive case: draw 4 sub-legs like:  \/
                c.scale(1/3,1/3);   // Sub-legs are 1/3rd the size of this leg
                leg(n-1);           // Recurse for the first sub-leg
                c.rotate(60*deg);   // Turn 60 degrees clockwise
                leg(n-1);           // Second sub-leg
                c.rotate(-120*deg); // Rotate 120 degrees back
                leg(n-1);           // Third sub-leg
                c.rotate(60*deg);   // Rotate back to our original heading
                leg(n-1);           // Final sub-leg
            }
            c.restore();            // Restore the transformation
            c.translate(len, 0);    // But translate to make end of leg (0,0)
        }
    }

  }


  class BytecodeInterpreter_JS extends BytecodeInterpreter_TIR_Opt {
    override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m) {
      override def isVolatile(field: ResolvedJavaField) = false // don't honor volatile
    }
    override def checkCastInternal(typ: ResolvedJavaType, value: Rep[Object]): Rep[Object] = value // no casts in JavaScript

    addClassDelegate(classOf[Program.JS]) { (frame, method, arguments) =>  // args: Array[Rep[Object]]

      val (receiver, args) = (arguments(0), arguments.drop(1))

      reflect[Object](""+receiver+"."+method.getName+"("+args.mkString(",")+")")
    }
  }


  def testA = withOutFileChecked(prefix+"A") {

    val it = new BytecodeInterpreter_JS2
    //it.TRACE = true
    //it.TRACE_BYTE_CODE = true
    //it.emitControlFlow = false
    //it.debugBlocks = true
    it.emitRecursive = true
    it.emitCSE = false
    it.initialize()
    try {
    val f = it.compile((x:Int) => {Program.draw; 0})
    } catch { case _: ClassNotFoundException =>  // we don't expect this to compile ...
    }
  }

  // TODO: html header / footer, val def syntax
  // TODO: remove null receiver for `document` var
  // TODO: generate `snowflake` and `leg` as JS methods instead of unfolding everything
  // TODO: remove null receiver for `document` var

  val header =
"""<html>
  <head>
    <title>Koch</title>
    <script type="text/javascript" src="koch.js"></script>
    <script type="text/javascript">
      function run(){"""

  val footer =
"""      }
    </script>
  </head>
  <body onLoad="run();">
    <canvas id="canvas" width="1000" height="500"></canvas>
  </body>
</html>"""


  class BytecodeInterpreter_JS2 extends BytecodeInterpreter_TIR_Opt {
    override def getRuntimeInterface(m: MetaAccessProvider) = new Runtime_Opt(m) {
      override def isVolatile(field: ResolvedJavaField) = false // don't honor volatile
    }

    override def isSafeRead(base: Object, offset: Long, field: ResolvedJavaField, typ: TypeRep[_]): Boolean = {
      // TODO: we should have a more general scheme for lazy vals
      if (field.getDeclaringClass.toJava == classOf[TestInterpreter2]) true
      else super.isSafeRead(base,offset,field,typ)
    }
    def handleJsMethod(parent: InterpreterFrame, m: ResolvedJavaMethod): Boolean = {
      val holder = m.getDeclaringClass
      if (classOf[Program.JS].isAssignableFrom(holder.toJava())) {
        //println("*** XXX JSM " + classOf[Program.JS] + " / " + holder.toJava)
        val receiver = parent.peekReceiver(m)
        val parameters = popArgumentsAsObject(parent, m, true)
        val returnValue = reflect[Object](""+receiver+"."+m.getName+"("+parameters.mkString(",")+")")
        pushAsObject(parent, m.getSignature().getReturnKind(), returnValue)
        true
      } else false
    }
    override def resolveAndInvoke(parent: InterpreterFrame, m: ResolvedJavaMethod): InterpreterFrame =
      if (handleJsMethod(parent,m)) null else super.resolveAndInvoke(parent, m)
    override def invokeDirect(parent: InterpreterFrame, m: ResolvedJavaMethod, hasReceiver: Boolean): InterpreterFrame =
      if (handleJsMethod(parent,m)) null else super.invokeDirect(parent, m, hasReceiver)
    override def checkCastInternal(typ: ResolvedJavaType, value: Rep[Object]): Rep[Object] = value // no casts in JavaScript

  }

}