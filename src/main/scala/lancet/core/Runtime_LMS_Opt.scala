/*
 * Copyright (c) 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package lancet.core


import java.lang.reflect.{Array=>jlrArray,_};

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;



trait RuntimeUniverse_LMS_Opt extends Core_LMS_Opt with RuntimeUniverse_LMS {

object static extends HasUnsafe
object unsafe extends Unsafe_Opt


var debugReadWrite = false
var debugNullCheck = false


trait Unsafe_Opt extends Unsafe_LMS {
}



class Runtime_Generic(metaProvider: MetaAccessProvider) extends Runtime_LMS(metaProvider) {

    def getField[T:TypeRep](base: Rep[Object], field: ResolvedJavaField): Rep[T] = {
        /*val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getObjectVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getObject(resolveBase(base, field), offset)
        }*/
        getFieldDefault[T](base, field)
    }

    def setField[T:TypeRep](value: Rep[T], base: Rep[Object], field: ResolvedJavaField): Unit = {
        /*val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putDouble(resolveBase(base, field), offset, value);
        }*/
        setFieldDefault[T](value, base, field)
    }

    def getArray[T:TypeRep](index: Rep[Long], array: Rep[Object]): Rep[T] = {
        /*checkArray(array, index);
        return unsafe.getByte(array, (Unsafe.ARRAY_BYTE_BASE_OFFSET) + Unsafe.ARRAY_BYTE_INDEX_SCALE.toLong * index);*/
        getArrayDefault[T](index, array)
    }

    def setArray[T:TypeRep:Manifest](value: Rep[T], index: Rep[Long], array: Rep[Object]): Unit = {
        /*checkArray(array, index);
        checkArrayType(array, classOf[T]);
        unsafe.putLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index, value);*/
        setArrayDefault[T](value, index, array)
    }

    def getFieldDefault[T:TypeRep](base: Rep[Object], field: ResolvedJavaField): Rep[T] = {
      typeRep[T].toString match {
        case "Int" => super.getFieldInt(base, field).asInstanceOf[Rep[T]]
        case "Long" => super.getFieldLong(base, field).asInstanceOf[Rep[T]]
        case "Short" => super.getFieldShort(base, field).asInstanceOf[Rep[T]]
        case "Byte" => super.getFieldByte(base, field).asInstanceOf[Rep[T]]
        case "Boolean" => super.getFieldBoolean(base, field).asInstanceOf[Rep[T]]
        case "Char" => super.getFieldChar(base, field).asInstanceOf[Rep[T]]
        case "Float" => super.getFieldFloat(base, field).asInstanceOf[Rep[T]]
        case "Double" => super.getFieldDouble(base, field).asInstanceOf[Rep[T]]
        case "Object" => super.getFieldObject(base, field).asInstanceOf[Rep[T]]
        case "java.lang.Object" => super.getFieldObject(base, field).asInstanceOf[Rep[T]]
      }
    }

    def setFieldDefault[T:TypeRep](value: Rep[T], base: Rep[Object], field: ResolvedJavaField): Unit = {
      typeRep[T].toString match {
        case "Int" => super.setFieldInt(value.asInstanceOf[Rep[Int]], base, field)
        case "Long" => super.setFieldLong(value.asInstanceOf[Rep[Long]], base, field)
        //case "Short" => super.setFieldShort(value.asInstanceOf[Rep[Short]], base, field)
        //case "Byte" => super.setFieldByte(value.asInstanceOf[Rep[Byte]], base, field)
        //case "Boolean" => super.setFieldBoolean(value.asInstanceOf[Rep[Boolean]], base, field)
        //case "Char" => super.setFieldChar(value.asInstanceOf[Rep[Char]], base, field)
        case "Float" => super.setFieldFloat(value.asInstanceOf[Rep[Float]], base, field)
        case "Double" => super.setFieldDouble(value.asInstanceOf[Rep[Double]], base, field)
        case "Object" => super.setFieldObject(value.asInstanceOf[Rep[Object]], base, field)
        case "java.lang.Object" => super.setFieldObject(value.asInstanceOf[Rep[Object]], base, field)
      }
    }

    def getArrayDefault[T:TypeRep](index: Rep[Long], array: Rep[Object]): Rep[T] = {
      typeRep[T].toString match {
        case "Int" => super.getArrayInt(index, array).asInstanceOf[Rep[T]]
        case "Long" => super.getArrayLong(index, array).asInstanceOf[Rep[T]]
        case "Short" => super.getArrayShort(index, array).asInstanceOf[Rep[T]]
        case "Byte" => super.getArrayByte(index, array).asInstanceOf[Rep[T]]
        //case "Boolean" => super.getArrayBoolean(index, array).asInstanceOf[Rep[T]]
        case "Char" => super.getArrayChar(index, array).asInstanceOf[Rep[T]]
        case "Float" => super.getArrayFloat(index, array).asInstanceOf[Rep[T]]
        case "Double" => super.getArrayDouble(index, array).asInstanceOf[Rep[T]]
        case "Object" => super.getArrayObject(index, array).asInstanceOf[Rep[T]]
        case "java.lang.Object" => super.getArrayObject(index, array).asInstanceOf[Rep[T]]
      }
    }

    def setArrayDefault[T:TypeRep](value: Rep[T], index: Rep[Long], array: Rep[Object]): Unit = {
      typeRep[T].toString match {
        case "Int" => super.setArrayInt(value.asInstanceOf[Rep[Int]], index, array)
        case "Long" => super.setArrayLong(value.asInstanceOf[Rep[Long]], index, array)
        case "Short" => super.setArrayShort(value.asInstanceOf[Rep[Short]], index, array)
        case "Byte" => super.setArrayByte(value.asInstanceOf[Rep[Byte]], index, array)
        //case "Boolean" => super.setArrayBoolean(value.asInstanceOf[Rep[Boolean]], index, array)
        case "Char" => super.setArrayChar(value.asInstanceOf[Rep[Char]], index, array)
        case "Float" => super.setArrayFloat(value.asInstanceOf[Rep[Float]], index, array)
        case "Double" => super.setArrayDouble(value.asInstanceOf[Rep[Double]], index, array)
        case "Object" => super.setArrayObject(value.asInstanceOf[Rep[Object]], index, array)
        case "java.lang.Object" => super.setArrayObject(value.asInstanceOf[Rep[Object]], index, array)
      }
    }


    override def getFieldObject(base: Rep[Object], field: ResolvedJavaField): Rep[AnyRef] = getField[Object](base,field)
    override def getFieldBoolean(base: Rep[Object], field: ResolvedJavaField): Rep[Boolean] = getField[Boolean](base,field)
    override def getFieldByte(base: Rep[Object], field: ResolvedJavaField): Rep[Byte] = getField[Byte](base,field)
    override def getFieldChar(base: Rep[Object], field: ResolvedJavaField): Rep[Char] = getField[Char](base,field)
    override def getFieldShort(base: Rep[Object], field: ResolvedJavaField): Rep[Short] = getField[Short](base,field)
    override def getFieldInt(base: Rep[Object], field: ResolvedJavaField): Rep[Int] = getField[Int](base,field)
    override def getFieldLong(base: Rep[Object], field: ResolvedJavaField): Rep[Long] = getField[Long](base,field)
    override def getFieldDouble(base: Rep[Object], field: ResolvedJavaField): Rep[Double] = getField[Double](base,field)
    override def getFieldFloat(base: Rep[Object], field: ResolvedJavaField): Rep[Float] = getField[Float](base,field)

    override def setFieldObject(value: Rep[Object], base: Rep[Object], field: ResolvedJavaField): Unit = setField[Object](value,base,field)
    override def setFieldInt(value: Rep[Int], base: Rep[Object], field: ResolvedJavaField): Unit = setField[Int](value,base,field)
    override def setFieldFloat(value: Rep[Float], base: Rep[Object], field: ResolvedJavaField): Unit = setField[Float](value,base,field)
    override def setFieldDouble(value: Rep[Double], base: Rep[Object], field: ResolvedJavaField): Unit = setField[Double](value,base,field)
    override def setFieldLong(value: Rep[Long], base: Rep[Object], field: ResolvedJavaField): Unit = setField[Long](value,base,field)
    
    override def getArrayByte(index: Rep[Long], array: Rep[Object]): Rep[Byte] = getArray[Byte](index,array)
    override def getArrayChar(index: Rep[Long], array: Rep[Object]): Rep[Char] = getArray[Char](index,array)
    override def getArrayShort(index: Rep[Long], array: Rep[Object]): Rep[Short] = getArray[Short](index,array)
    override def getArrayInt(index: Rep[Long], array: Rep[Object]): Rep[Int] = getArray[Int](index,array)
    override def getArrayLong(index: Rep[Long], array: Rep[Object]): Rep[Long] = getArray[Long](index,array)
    override def getArrayDouble(index: Rep[Long], array: Rep[Object]): Rep[Double] = getArray[Double](index,array)
    override def getArrayFloat(index: Rep[Long], array: Rep[Object]): Rep[Float] = getArray[Float](index,array)
    override def getArrayObject(index: Rep[Long], array: Rep[Object]): Rep[Object] = getArray[Object](index,array)
    
    override def setArrayByte(value: Rep[Byte], index: Rep[Long], array: Rep[Object]): Unit = setArray[Byte](value, index, array)
    override def setArrayChar(value: Rep[Char], index: Rep[Long], array: Rep[Object]): Unit = setArray[Char](value, index, array)
    override def setArrayShort(value: Rep[Short], index: Rep[Long], array: Rep[Object]): Unit = setArray[Short](value, index, array)
    override def setArrayInt(value: Rep[Int], index: Rep[Long], array: Rep[Object]): Unit = setArray[Int](value, index, array)
    override def setArrayLong(value: Rep[Long], index: Rep[Long], array: Rep[Object]): Unit = setArray[Long](value, index, array)
    override def setArrayFloat(value: Rep[Float], index: Rep[Long], array: Rep[Object]): Unit = setArray[Float](value, index, array)
    override def setArrayDouble(value: Rep[Double], index: Rep[Long], array: Rep[Object]): Unit = setArray[Double](value, index, array)
    override def setArrayObject(value: Rep[Object], index: Rep[Long], array: Rep[Object]): Unit = setArray[Object](value, index, array)

}





  // overridable ...
  def isSafeRead(base: Object, offset: Long, field: ResolvedJavaField, typ: TypeRep[_]): Boolean = {
    if (base == null) {
      emitString("// base is null, dammit"); false
    } else {
/*
      val unsafePrefixes = "sun.nio."::"java.io."::"java.nio."::Nil
      val safeFields = "java.io.PrintStream.16"::
                       "java.io.PrintStream.40"::
                       "java.io.PrintStream.48"::
                       "java.io.BufferedWriter.32":: /*.40 Int .44 Int */
                       "java.io.BufferedWriter.48"::
                       "java.io.BufferedWriter.56"::
                       "java.io.BufferedWriter.64"::
                       "java.io.OutputStreamWriter.40"::
                       "sun.nio.cs.StreamEncoder.32"::
                       "sun.nio.cs.StreamEncoder.56"::
                       "sun.nio.cs.StreamEncoder.64"::
                       "sun.nio.cs.StreamEncoder.72"::
                       "sun.nio.cs.StreamEncoder.80"::
                       "sun.nio.cs.StreamEncoder.88"::
                       "java.nio.HeapByteBuffer[pos=0 lim=8192 cap=8192].48":: //HACK/FIXME
                       "sun.nio.cs.UTF_8$Encoder.72"::
                       "java.nio.charset.CoderResult$1.16"::
                       Nil
      val str = base.toString
      val lookup = str.replaceAll("@[a-z0-9]+","")+"."+offset
      //println("// " + lookup + "/" + safeFields)
      val r = lookup.startsWith("Class.forName") || safeFields.contains(lookup) || !unsafePrefixes.exists(str startsWith _)
      if (debugReadWrite) {
        val pred = if (r) "safe" else "unsafe"
        emitString("// " + pred + " read: " + base.toString.replace("\n","\\n") + "." + offset + ":" + typ)
      }
      r
*/

      if (Modifier.isFinal(field.getModifiers)) return true

      val name = field.getDeclaringClass.toJava.getName + "." + field.getName

      name match {
        case "java.io.FilterOutputStream.out" => true
        case "java.io.PrintStream.charOut" => true
        case "java.io.PrintStream.textOut" => true
        case "java.io.Writer.lock" => true
        case "java.io.BufferedWriter.out" => true
        case "java.io.BufferedWriter.cb" => true
        case "sun.nio.cs.StreamEncoder.encoder" => true
        case "sun.nio.cs.StreamEncoder.bb" => true
        case "java.nio.charset.CharsetEncoder.stateNames" => true
        case _ =>
         false
      }
    }
  }

  def isSafeReadArray(base: Object, typ: TypeRep[_]): Boolean = {
    false
  }



val writeAfterWrite: List[(Rep[Any],Rep[Any])] = Nil
val readAfterWrite: List[(Rep[Any],Rep[Any])] = Nil
val writeAfterRead: List[(Rep[Any],Rep[Any])] = Nil




class Runtime_Opt(metaProvider: MetaAccessProvider) extends Runtime_Generic(metaProvider) {



    /*def invoke(method: ResolvedJavaMethod, args: Array[Rep[Object]]): Rep[Object] =
        reflect(""+method+".invoke("+args.mkString(",")+")")

    def typeIsInstance(typ: ResolvedJavaType, value: Rep[Object]): Rep[Boolean] = {
        reflect(""+value+".isInstanceOf["+typ.toJava+"]")
    }

    def monitorEnter(value: Rep[Object]): Unit = {
        nullCheck(value)
        unsafe.monitorEnter(value)
    }

    def monitorExit(value: Rep[Object]): Unit = {
        nullCheck(value)
        unsafe.monitorEnter(value)
    }
*/

    override def newObject(typ: ResolvedJavaType): Rep[Object] = { //} throws InstantiationException {
      val clazz = typ.toJava
      import scala.collection.immutable.Map

      val r@Dyn(s) = super.newObject(typ)
      //rewrite(s+" eq null", Static(false))
      store += (s -> Partial(Map("alloc" -> r, "clazz" -> unit(clazz))))
      r
    }



    override def newArray(typ: ResolvedJavaType, size: Rep[Int]): Rep[Object] = { // throws InstantiationException {
      import scala.collection.immutable.Map

      val rs = size

      // FIXME: size may be extracted later and may point to a stack var

      //println("// array size " + size + "=" + rs + "=" + eval(size))

      val r@Dyn(s) = super.newArray(typ, size)
      store += (s -> Partial(Map("alloc" -> r, "clazz" -> unit(typ.getArrayClass.toJava), "size" -> rs)))
      r
    }

    override def newArray(typ: Class[_], size: Rep[Int]): Rep[Object] = { // throws InstantiationException {
      import scala.collection.immutable.Map

      val rs = size

      // FIXME: size may be extracted later and may point to a stack var

      //println("// array size " + size + "=" + rs + "=" + eval(size))

      val r@Dyn(s) = super.newArray(typ, size)
      //rewrite(s+" eq null", Static(false))
      store += (s -> Partial(Map("alloc" -> r, "clazz" -> unit(java.lang.reflect.Array.newInstance(typ, 0).getClass), "size" -> rs)))
      r
    }

    override def newMultiArray(typ: ResolvedJavaType, dimensions: Array[Rep[Int]]): Rep[Object] = { // throws InstantiationException {
      import scala.collection.immutable.Map

      val r@Dyn(s) = super.newMultiArray(typ, dimensions)
      //rewrite(s+" eq null", Static(false))
      store += (s -> Partial(Map("alloc" -> r, "clazz" -> unit(java.lang.reflect.Array.newInstance(typ.toJava, dimensions.map(_=>0):_*).getClass))))
      r
    }



    override def nullCheck(value: Rep[Object]): Rep[Object] = {
      if (debugNullCheck) emitString("// nullcheck "+value + "=" + eval(value))
      super.nullCheck(value)
    }

/*
    def nullCheck(value: Rep[Object]): Rep[Object] = 
      if_(value === unit(null)) (reflect[Object]("throw new NullPointerException()")) (value)

    def checkArrayType(array: Rep[Object], arrayType: Class[_]): Unit = reflect("""{
        if (arrayType == null) {
            return;
        }
        val typ: ResolvedJavaType = metaProvider.lookupJavaType(array.getClass()).getComponentType();
        if (!typ.toJava().isAssignableFrom(arrayType)) {
            throw new ArrayStoreException(arrayType.getName());
        }
    }""")

    def checkArray(array: Rep[Object], index: Rep[Long]): Unit = reflect("""{
        nullCheck(array);
        val typ: ResolvedJavaType = metaProvider.lookupJavaType(array.getClass());
        if (!typ.isArray()) {
            throw new ArrayStoreException(array.getClass().getName());
        }
        if (index < 0 || index >= arrayLength(array)) {
            throw new ArrayIndexOutOfBoundsException(index.toInt);
        }
    }""")
    */




    def getFieldUnsafe[T:TypeRep](base: Object, offset: Long): T = {
      typeRep[T].toString match {
        case "Int" => static.unsafe.getInt(base, offset).asInstanceOf[T]
        case "Short" => static.unsafe.getShort(base, offset).asInstanceOf[T]
        case "Byte" => static.unsafe.getByte(base, offset).asInstanceOf[T]
        case "Boolean" => static.unsafe.getBoolean(base, offset).asInstanceOf[T]
        case "Char" => static.unsafe.getChar(base, offset).asInstanceOf[T]
        case "Float" => static.unsafe.getFloat(base, offset).asInstanceOf[T]
        case "Double" => static.unsafe.getDouble(base, offset).asInstanceOf[T]
        case "Object" => static.unsafe.getObject(base, offset).asInstanceOf[T]
      }
    }

    // hack: used by base_opt / regular getField will check store for updates
    def getFieldConst[T:TypeRep](base0: Rep[Object], field: ResolvedJavaField): Rep[T] = {
      val offset = resolveOffset(field)
      val Static(base) = resolveBase(base0,field)
      if (isSafeRead(base, offset, field, typeRep[T]))
        liftConst(getFieldUnsafe[T](base,offset))
      else
        super.getField[T](base0, field)
    }

    override def getField[T:TypeRep](base0: Rep[Object], field: ResolvedJavaField): Rep[T] = {
      val offset = resolveOffset(field)
      val base = resolveBase(base0,field)
      val volatile = isVolatile(field) // TODO!

      if (debugReadWrite) emitString("// getField " + base0 + ":" + field.getDeclaringClass.toJava.getName + "." + field.getName)

      if (base+","+offset == "Class.forName(\"sun.misc.VM\").asInstanceOf[Object],261.asInstanceOf[Long]") return unit(true).asInstanceOf[Rep[T]]

      /*
        value = base.field

        let S/T be the set of all objects base may/must point to:
        this statement may/must read from all s/t in S/T

        value may/must point to all s/t in S/T

        write after read (soft):  for all statements that may read from s.field where s in S
        write after write (soft): for all statements that may write to  s.field where s in S
      */



      (eval(base), eval(offset)) match {
          case (VConst(base), VConst(offset)) if isSafeRead(base, offset, field, typeRep[T]) => 
            liftConst(getFieldUnsafe[T](base,offset))
          case (Partial(fs), VConst(offset)) => 
            def default = fs("alloc") match {
              case Static(base1:Object) => 
                if (isSafeRead(base1, offset, field, typeRep[T])) liftConst[T](getFieldUnsafe[T](base1,offset))
                else super.getField[T](base0, field)
              case _ => liftConst[T](null.asInstanceOf[T])
            }
            val k = field.getName//offset.toString
            if (typeRep[T] == typeRep[Boolean])
              fs.get(k).
                map{x=>(if(x.typ==typeRep[Int]) (x.asInstanceOf[Rep[Int]] === 1) else x).asInstanceOf[Rep[T]]}.
                getOrElse(default) // value stored will be int, not bool (not quite sure why exactly)
            else
              fs.getOrElse(k, default).asInstanceOf[Rep[T]]
          case _ => super.getField[T](base0, field)
        }
        /*if (isVolatile(field)) {
            unsafe.getObjectVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getObject(resolveBase(base, field), offset)
        }*/
    }


    override def setField[T:TypeRep](value: Rep[T], base0: Rep[Object], field: ResolvedJavaField): Unit = {
      val offset = resolveOffset(field)
      val base = resolveBase(base0,field)
      val volatile = isVolatile(field) // TODO!

      /*
        base.field = value

        let S/T be the set of all objects base may/must point to:
        this statement may/must write to all s/t in S/T

        let U/V be the set of all objects value may/must point to
        base.field may/must point to u/v in U/V

        write after read (soft):  for all statements that may read from s.field where s in S
        write after write (soft): for all statements that may write to  s.field where s in S
      */

      super.setField(value, base0, field)
      //val Static(off) = offset
      val k = field.getName // off.toString

      val s = base match { case Static(x) => VConstToString(x) case x => quote(x) }
      val Partial(fs) = eval(base) match {
        case s@Partial(_) => s
        case s@VConst(c) => Partial(Map("alloc" -> Static(c), "clazz" -> Static(c.getClass)))
        // Alias: update all partials with lub value for field
        case s => emitString("ERROR // write to unknown: " + s); return // need to trash the whole store?? sigh ...
      }
      store += (s -> Partial(fs + (k -> value)))

      if (debugReadWrite) emitString("// storing: " + (s -> Partial(fs + (k -> value))))

      /*val offset = resolveOffset(field);
      if (isVolatile(field)) {
          unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
      } else {
          unsafe.putDouble(resolveBase(base, field), offset, value);
      }*/
    }

    override def getArray[T:TypeRep](index: Rep[Long], array: Rep[Object]): Rep[T] = {
      checkArray(array, index);

      val base = array
      val offset = 0

      (eval(base), eval(index)) match {
          case (VConst(base:Array[T]), VConst(index)) if isSafeReadArray(base, typeRep[T]) => 
            liftConst(base(index.toInt))
          case (Partial(fs), VConst(index)) => 
            def default = fs("alloc") match {
              case Static(base1:Array[T]) => 
                if (isSafeReadArray(base1, typeRep[Boolean])) liftConst[T](base1(index.toInt))
                else super.getArray[T](index, array)
              case _ => liftConst[T](null.asInstanceOf[T])
            }
            if (typeRep[T] == typeRep[Boolean])
              fs.get(offset.toString).
                map{x=>(if(x.typ==typeRep[Int]) (x.asInstanceOf[Rep[Int]] === 1) else x).asInstanceOf[Rep[T]]}.
                getOrElse(default) // value stored will be int, not bool (not quite sure why exactly)
            else
              fs.getOrElse(offset.toString, default).asInstanceOf[Rep[T]]
          case _ => super.getArray[T](index, array)
        }
        /*checkArray(array, index);
        return unsafe.getByte(array, (Unsafe.ARRAY_BYTE_BASE_OFFSET) + Unsafe.ARRAY_BYTE_INDEX_SCALE.toLong * index);*/
    }

    override def setArray[T:TypeRep:Manifest](value: Rep[T], index: Rep[Long], array: Rep[Object]): Unit = {
      checkArray(array, index)
      checkArrayType(array, manifest[T].erasure)

      val offset = index
      val base = array

      super.setArray(value, index,array)
      //val Static(off) = offset
      //val off = offset

/*
      val s = dealias(base) match { case Static(x) => VConstToString(x) case x => x.toString }
      val Partial(fs) = eval(base) match {
        case s@Partial(_) => s
        case s@VConst(c) => Partial(Map("alloc" -> Static(c), "clazz" -> Static(c.getClass), "size" -> Static(c.length)))
        // Alias: update all partials with lub value for field
        case s => emitString("ERROR // write to unknown: " + s); return // need to trash the whole store?? sigh ...
      }
      store += (s -> Partial(fs + ("array" -> dealias(value))))

      if (debugReadWrite) emitString("// storing: " + (s -> Partial(fs + ("array" -> value))))
*/

      /*checkArray(array, index);
      checkArrayType(array, classOf[T]);
      unsafe.putLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index, value);*/
    }




    // TODO: Partial of VConst
    override def arrayLength(array: Rep[Object]): Rep[Int] = eval(array) match {
      case VConst(array) => java.lang.reflect.Array.getLength(array)
      case Partial(fs) => fs("size").asInstanceOf[Rep[Int]]
      case _ => super.arrayLength(array)
    }


    // TODO: Partial of VConst
    override def objectGetClass(base: Rep[Object]): Rep[Class[Object]] = eval(base) match {
      case VConst(base) => unit(base.getClass).asInstanceOf[Rep[Class[Object]]]
      case Partial(fs) => 
        fs.getOrElse("clazz", {
          val alloc = fs("alloc").asInstanceOf[Rep[Object]]
          if (alloc == base) super.objectGetClass(base)
          else objectGetClass(alloc)
        }).asInstanceOf[Rep[Class[Object]]]
      case _ => super.objectGetClass(base)
    }

    override def classGetName(base: Rep[Class[Object]]): Rep[String] = eval(base) match {
      case VConst(base) => unit(base.getName).asInstanceOf[Rep[String]]
      case _ => super.classGetName(base)
    }
    override def classIsArray(base: Rep[Class[Object]]): Rep[Boolean] = eval(base) match {
      case VConst(base) => unit(base.isArray)
      case _ => super.classIsArray(base)
    }
    override def classGetComponentType(base: Rep[Class[Object]]): Rep[Class[Object]] = eval(base) match {
      case VConst(base) => unit(base.getComponentType).asInstanceOf[Rep[Class[Object]]]
      case _ => super.classGetComponentType(base)
    }
    override def classIsAssignableFrom(base: Rep[Class[Object]], other: Rep[Class[Object]]): Rep[Boolean] = (eval(base),eval(other)) match {
      case (VConst(base),VConst(other)) => unit(base.isAssignableFrom(other))
      case _ => super.classIsAssignableFrom(base,other)
    }

}

}
