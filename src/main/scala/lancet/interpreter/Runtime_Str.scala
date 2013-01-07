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
package playground.interpreter


import java.lang.reflect.{Array=>jlrArray,_};
import java.util._;
import sun.misc._;

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;


trait RuntimeUniverse_Simple extends Core_Simple with RuntimeUniverse_Str {

  trait Unsafe_Simple extends Unsafe_Str
  object unsafe extends Unsafe_Simple
}


trait RuntimeUniverse_Str extends Core_Str with RuntimeUniverse {

def unsafe: Unsafe_Str

trait Unsafe_Str {

  def monitorEnter(value: Rep[Object]): Rep[Unit] = 
    reflect[Unit]("unsafe.monitorEnter("+value+")")
  def monitorExit(value: Rep[Object]): Rep[Unit] = 
    reflect[Unit]("unsafe.monitorExit("+value+")")

  def getObject(base: Rep[Object], offset: Rep[Long]): Rep[Object] = 
    reflect[Object]("unsafe.getObject("+base+","+offset+")")
  def getObjectVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Object] = 
    reflect[Object]("unsafe.getObjectVolatile("+base+","+offset+")")

  def getBoolean(base: Rep[Object], offset: Rep[Long]): Rep[Boolean] = 
    reflect[Boolean]("unsafe.getBoolean("+base+","+offset+")")
  def getBooleanVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Boolean] = 
    reflect[Boolean]("unsafe.getBooleanVolatile("+base+","+offset+")")

  def getByte(base: Rep[Object], offset: Rep[Long]): Rep[Byte] = 
    reflect[Byte]("unsafe.getByte("+base+","+offset+")")
  def getByteVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Byte] = 
    reflect[Byte]("unsafe.getByteVolatile("+base+","+offset+")")

  def getChar(base: Rep[Object], offset: Rep[Long]): Rep[Char] = 
    reflect[Char]("unsafe.getChar("+base+","+offset+")")
  def getCharVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Char] = 
    reflect[Char]("unsafe.getCharVolatile("+base+","+offset+")")

  def getShort(base: Rep[Object], offset: Rep[Long]): Rep[Short] = 
    reflect[Short]("unsafe.getShort("+base+","+offset+")")
  def getShortVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Short] = 
    reflect[Short]("unsafe.getShortVolatile("+base+","+offset+")")

  def getInt(base: Rep[Object], offset: Rep[Long]): Rep[Int] = 
    reflect[Int]("unsafe.getInt("+base+","+offset+")")
  def getIntVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Int] = 
    reflect[Int]("unsafe.getIntVolatile("+base+","+offset+")")

  def getLong(base: Rep[Object], offset: Rep[Long]): Rep[Long] = 
    reflect[Long]("unsafe.getLong("+base+","+offset+")")
  def getLongVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Long] = 
    reflect[Long]("unsafe.getLongVolatile("+base+","+offset+")")

  def getFloat(base: Rep[Object], offset: Rep[Long]): Rep[Float] = 
    reflect[Float]("unsafe.getFloat("+base+","+offset+")")
  def getFloatVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Float] = 
    reflect[Float]("unsafe.getFloatVolatile("+base+","+offset+")")

  def getDouble(base: Rep[Object], offset: Rep[Long]): Rep[Double] = 
    reflect[Double]("unsafe.getDouble("+base+","+offset+")")
  def getDoubleVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Double] = 
    reflect[Double]("unsafe.getDoubleVolatile("+base+","+offset+")")


  def putObject(base: Rep[Object], offset: Rep[Long], value: Rep[Object]): Rep[Unit] = 
    reflect[Unit]("unsafe.putObject("+base+","+offset+", "+value+")")
  def putObjectVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Object]): Rep[Unit] = 
    reflect[Unit]("unsafe.putObjectVolatile("+base+","+offset+", "+value+")")

  def putBoolean(base: Rep[Object], offset: Rep[Long], value: Rep[Boolean]): Rep[Unit] = 
    reflect[Unit]("unsafe.putBoolean("+base+","+offset+", "+value+")")
  def putBooleanVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Boolean]): Rep[Unit] = 
    reflect[Unit]("unsafe.putBooleanVolatile("+base+","+offset+", "+value+")")

  def putByte(base: Rep[Object], offset: Rep[Long], value: Rep[Byte]): Rep[Unit] = 
    reflect[Unit]("unsafe.putByte("+base+","+offset+", "+value+")")
  def putByteVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Byte]): Rep[Unit] = 
    reflect[Unit]("unsafe.putByteVolatile("+base+","+offset+", "+value+")")

  def putChar(base: Rep[Object], offset: Rep[Long], value: Rep[Char]): Rep[Unit] = 
    reflect[Unit]("unsafe.putChar("+base+","+offset+", "+value+")")
  def putCharVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Char]): Rep[Unit] = 
    reflect[Unit]("unsafe.putCharVolatile("+base+","+offset+", "+value+")")

  def putShort(base: Rep[Object], offset: Rep[Long], value: Rep[Short]): Rep[Unit] = 
    reflect[Unit]("unsafe.putShort("+base+","+offset+", "+value+")")
  def putShortVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Short]): Rep[Unit] = 
    reflect[Unit]("unsafe.putShortVolatile("+base+","+offset+", "+value+")")

  def putInt(base: Rep[Object], offset: Rep[Long], value: Rep[Int]): Rep[Unit] = 
    reflect[Unit]("unsafe.putInt("+base+","+offset+", "+value+")")
  def putIntVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Int]): Rep[Unit] = 
    reflect[Unit]("unsafe.putIntVolatile("+base+","+offset+", "+value+")")

  def putLong(base: Rep[Object], offset: Rep[Long], value: Rep[Long]): Rep[Unit] = 
    reflect[Unit]("unsafe.putLong("+base+","+offset+", "+value+")")
  def putLongVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Long]): Rep[Unit] = 
    reflect[Unit]("unsafe.putLongVolatile("+base+","+offset+", "+value+")")

  def putFloat(base: Rep[Object], offset: Rep[Long], value: Rep[Float]): Rep[Unit] = 
    reflect[Unit]("unsafe.putFloat("+base+","+offset+", "+value+")")
  def putFloatVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Float]): Rep[Unit] = 
    reflect[Unit]("unsafe.putFloatVolatile("+base+","+offset+", "+value+")")

  def putDouble(base: Rep[Object], offset: Rep[Long], value: Rep[Double]): Rep[Unit] = 
    reflect[Unit]("unsafe.putDouble("+base+","+offset+", "+value+")")
  def putDoubleVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Double]): Rep[Unit] = 
    reflect[Unit]("unsafe.putDoubleVolatile("+base+","+offset+", "+value+")")


  def allocateInstance(clazz: Class[_]): Rep[Object] = 
    reflect[Object]("unsafe.allocateInstance(Class.forName(\""+clazz.getName+"\"))")

}


class Runtime_Str(metaProvider: MetaAccessProvider) extends Runtime {

    val toJavaM = classOf[HotSpotResolvedJavaMethod].getDeclaredMethod("toJava")
    toJavaM.setAccessible(true)

    def invoke(method: ResolvedJavaMethod, args: Array[Rep[Object]]): Rep[Object] = {
        
        val holder = method.holder.toJava.getName
        val name = method.name

        def methToJava(m: ResolvedJavaMethod) = toJavaM.invoke(method).asInstanceOf[java.lang.reflect.Method]

        val m = methToJava(method)
        m.setAccessible(true)

        val static = Modifier.isStatic(method.accessFlags)

        val mtyp = TypeRep[Object](method.signature.returnKind.toString match {  //TODO: cleanup
            case "void" => "Unit"
            case "int" => "Int"
            case _ => "Object"
        })

        if (!static) {
            if (args.length > 1)
                reflect[Object](liftConst(m),".invoke("+args.map(_+".asInstanceOf[AnyRef]").mkString(",")+") // "+holder+"."+name)(mtyp)
            else
                reflect[Object](liftConst(m),".invoke("+args(0)+") // "+holder+"."+name)(mtyp)
            //reflect[Object](args(0)+".asInstanceOf["+holder+"]."+name+"("+args.drop(1).mkString(",")+").asInstanceOf[Object]")
        } else {
            reflect[Object](holder+"."+name+"("+args.mkString(",")+").asInstanceOf[Object]")(mtyp)
        }


        // TODO: actual class as result type info?

        //reflect(""+method+".invoke("+args.mkString(",")+")")

    }

    def typeIsInstance(typ: ResolvedJavaType, value: Rep[Object]): Rep[Boolean] = {
        reflect[Boolean](""+value+".isInstanceOf["+typ.toJava+"]")
    }

    def monitorEnter(value: Rep[Object]): Unit = {
        nullCheck(value)
        unsafe.monitorEnter(value)
    }

    def monitorExit(value: Rep[Object]): Unit = {
        nullCheck(value)
        unsafe.monitorExit(value)
    }

    def newObject(typ: ResolvedJavaType): Rep[Object] = { //} throws InstantiationException {
        unsafe.allocateInstance(typ.toJava());
    }

    def newArray(typ: ResolvedJavaType, size: Rep[Int]): Rep[Object] = { // throws InstantiationException {
        reflect[Object]("new Array["+typ.toJava().getName+"]("+size+")");
    }

    def newArray(typ: Class[_], size: Rep[Int]): Rep[Object] = { // throws InstantiationException {
        reflect[Object]("new Array["+typ.getName+"]("+size+")");
    }

    def newMultiArray(typ: ResolvedJavaType, dimensions: Array[Rep[Int]]): Rep[Object] = { // throws InstantiationException {
        reflect[Object]("new Array["+typ.toJava().getName+"]("+dimensions.mkString(",")+")");
    }

    def getFieldObject(base: Rep[Object], field: ResolvedJavaField): Rep[AnyRef] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getObjectVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getObject(resolveBase(base, field), offset)
        }
    }

    def getFieldBoolean(base: Rep[Object], field: ResolvedJavaField): Rep[Boolean] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getBooleanVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getBoolean(resolveBase(base, field), offset)
        }
    }

    def getFieldByte(base: Rep[Object], field: ResolvedJavaField): Rep[Byte] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getByteVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getByte(resolveBase(base, field), offset)
        }
    }

    def getFieldChar(base: Rep[Object], field: ResolvedJavaField): Rep[Char] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getCharVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getChar(resolveBase(base, field), offset)
        }
    }

    def getFieldShort(base: Rep[Object], field: ResolvedJavaField): Rep[Short] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getShortVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getShort(resolveBase(base, field), offset)
        }
    }

    def getFieldInt(base: Rep[Object], field: ResolvedJavaField): Rep[Int] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getIntVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getInt(resolveBase(base, field), offset)
        }
    }

    def getFieldLong(base: Rep[Object], field: ResolvedJavaField): Rep[Long] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getLongVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getLong(resolveBase(base, field), offset)
        }
    }

    def getFieldDouble(base: Rep[Object], field: ResolvedJavaField): Rep[Double] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getDoubleVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getDouble(resolveBase(base, field), offset)
        }
    }

    def getFieldFloat(base: Rep[Object], field: ResolvedJavaField): Rep[Float] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getFloatVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getFloat(resolveBase(base, field), offset)
        }
    }

    def setFieldObject(value: Rep[Object], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putObjectVolatile(resolveBase(base, field), offset, value)
        } else {
            unsafe.putObject(resolveBase(base, field), offset, value)
        }
    }

    def setFieldInt(value: Rep[Int], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putIntVolatile(resolveBase(base, field), offset, value)
        } else {
            unsafe.putInt(resolveBase(base, field), offset, value)
        }
    }


    def setFieldFloat(value: Rep[Float], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putFloatVolatile(resolveBase(base, field), offset, value)
        } else {
            unsafe.putFloat(resolveBase(base, field), offset, value)
        }
    }

    def setFieldDouble(value: Rep[Double], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putDouble(resolveBase(base, field), offset, value);
        }
    }

    def setFieldLong(value: Rep[Long], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putDouble(resolveBase(base, field), offset, value);
        }
    }

    def getArrayByte(index: Rep[Long], array: Rep[Object]): Rep[Byte] = {
        checkArray(array, index);
        return unsafe.getByte(array, (Unsafe.ARRAY_BYTE_BASE_OFFSET) + Unsafe.ARRAY_BYTE_INDEX_SCALE.toLong * index);
    }

    def getArrayChar(index: Rep[Long], array: Rep[Object]): Rep[Char] = {
        checkArray(array, index);
        return unsafe.getChar(array, Unsafe.ARRAY_CHAR_BASE_OFFSET + Unsafe.ARRAY_CHAR_INDEX_SCALE * index);
    }

    def getArrayShort(index: Rep[Long], array: Rep[Object]): Rep[Short] = {
        checkArray(array, index);
        return unsafe.getShort(array, Unsafe.ARRAY_SHORT_BASE_OFFSET + Unsafe.ARRAY_SHORT_INDEX_SCALE * index);
    }

    def getArrayInt(index: Rep[Long], array: Rep[Object]): Rep[Int] = {
        checkArray(array, index);
        return unsafe.getInt(array, Unsafe.ARRAY_INT_BASE_OFFSET + Unsafe.ARRAY_INT_INDEX_SCALE * index);
    }

    def getArrayLong(index: Rep[Long], array: Rep[Object]): Rep[Long] = {
        checkArray(array, index);
        return unsafe.getLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index);
    }

    def getArrayDouble(index: Rep[Long], array: Rep[Object]): Rep[Double] = {
        checkArray(array, index);
        return unsafe.getDouble(array, Unsafe.ARRAY_DOUBLE_BASE_OFFSET + Unsafe.ARRAY_DOUBLE_INDEX_SCALE * index);
    }

    def getArrayFloat(index: Rep[Long], array: Rep[Object]): Rep[Float] = {
        checkArray(array, index);
        return unsafe.getFloat(array, Unsafe.ARRAY_FLOAT_BASE_OFFSET + Unsafe.ARRAY_FLOAT_INDEX_SCALE * index);
    }

    def getArrayObject(index: Rep[Long], array: Rep[Object]): Rep[Object] = {
        checkArray(array, index);
        return unsafe.getObject(array, Unsafe.ARRAY_OBJECT_BASE_OFFSET + Unsafe.ARRAY_OBJECT_INDEX_SCALE * index);
    }

    def setArrayByte(value: Rep[Byte], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        if (array.isInstanceOf[Array[Boolean]]) {
            checkArrayType(array, classOf[Boolean]);
        } else {
            checkArrayType(array, classOf[Byte]);
        }
        unsafe.putByte(array, Unsafe.ARRAY_BYTE_BASE_OFFSET + Unsafe.ARRAY_BYTE_INDEX_SCALE * index, value);
    }

    def setArrayChar(value: Rep[Char], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Char]);
        unsafe.putChar(array, Unsafe.ARRAY_CHAR_BASE_OFFSET + Unsafe.ARRAY_CHAR_INDEX_SCALE * index, value);
    }

    def setArrayShort(value: Rep[Short], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Short]);
        unsafe.putShort(array, Unsafe.ARRAY_SHORT_BASE_OFFSET + Unsafe.ARRAY_SHORT_INDEX_SCALE * index, value);
    }

    def setArrayInt(value: Rep[Int], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Int]);
        unsafe.putInt(array, Unsafe.ARRAY_INT_BASE_OFFSET + Unsafe.ARRAY_INT_INDEX_SCALE * index, value);
    }

    def setArrayLong(value: Rep[Long], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Long]);
        unsafe.putLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index, value);
    }

    def setArrayFloat(value: Rep[Float], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Float]);
        unsafe.putFloat(array, Unsafe.ARRAY_FLOAT_BASE_OFFSET + Unsafe.ARRAY_FLOAT_INDEX_SCALE * index, value);
    }

    def setArrayDouble(value: Rep[Double], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Double]);
        unsafe.putDouble(array, Unsafe.ARRAY_DOUBLE_BASE_OFFSET + Unsafe.ARRAY_DOUBLE_INDEX_SCALE * index, value);
    }

    def setArrayObject(value: Rep[Object], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, objectGetClass(value))
        unsafe.putObject(array, Unsafe.ARRAY_OBJECT_BASE_OFFSET + Unsafe.ARRAY_OBJECT_INDEX_SCALE * index, value);
    }

    def nullCheck(value: Rep[Object]): Rep[Object] = {
      if_(value === unit(null)) (reflect[Object]("throw new NullPointerException()")) (value)
      value // TBD: what's the result?
    }

    def checkArrayType(array: Rep[Object], arrayType: Class[_]): Unit = {
        val cond = objectGetClass(array).getComponentType().isAssignableFrom(liftConst(arrayType.asInstanceOf[Class[Object]]))
        //val cond = reflect[Boolean]("!"+array+".getClass().getComponentType().isAssignableFrom(classOf["+arrayType.getName+"])")
        if_(!cond) (reflect[Unit]("throw new ArrayStoreException(\""+arrayType.getName()+"\")")) (liftConst())
    }

    def checkArrayType(array: Rep[Object], arrayType: Rep[Class[Object]]): Unit = { //TODO: shouldn't duplicate
        val cond = objectGetClass(array).getComponentType().isAssignableFrom(arrayType)
        if_(!cond) (reflect[Unit]("throw new ArrayStoreException(\""+arrayType.getName()+"\")")) (liftConst())
    }


    def checkArray(array: Rep[Object], index: Rep[Long]): Unit = {
        nullCheck(array)
        val typ = objectGetClass(array)
        val cond = typ.isArray()
        if_(!cond) (reflect[Unit]("throw new ArrayStoreException("+typ.getName()+")")) (liftConst());
        if_(index < 0 || index >= arrayLength(array)) {
            reflect[Unit]("throw new ArrayIndexOutOfBoundsException("+index.toInt+")");
        } (liftConst())
    }

    def arrayLength(array: Rep[Object]): Rep[Int] = {
        assert(array != null);
        return reflect[Int]("java.lang.reflect.Array.getLength("+array+")");
    }

    def isVolatile(field: ResolvedJavaField): Boolean = {
        return Modifier.isVolatile(field.accessFlags());
    }

    def resolveOffset(field: ResolvedJavaField): Long = {
        return field.asInstanceOf[HotSpotResolvedJavaField].offset();
    }

    def resolveBase(base: Rep[Object], field: ResolvedJavaField): Rep[Object] =
      if (Modifier.isStatic(field.accessFlags)) unit(field.holder().toJava()) else base



    def objectGetClass(base: Rep[Object]): Rep[Class[Object]] = reflect[Class[Object]](base+".getClass.asInstanceOf[Class[Object]]")
    def classGetName(base: Rep[Class[Object]]): Rep[String] = reflect[String](base+".getName")
    def classIsArray(base: Rep[Class[Object]]): Rep[Boolean] = reflect[Boolean](base+".isArray")
    def classGetComponentType(base: Rep[Class[Object]]): Rep[Class[Object]] = reflect[Class[Object]](base+".getComponentType.asInstanceOf[Class[Object]]")
    def classIsAssignableFrom(base: Rep[Class[Object]], other: Rep[Class[Object]]): Rep[Boolean] = reflect[Boolean](base+".isAssignableFrom("+other+")")

}



}
