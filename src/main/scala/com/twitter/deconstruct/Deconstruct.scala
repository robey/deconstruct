package com.twitter.deconstruct

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

class ParseException(reason: String) extends Exception(reason)

private[deconstruct] sealed trait ConstantPoolEntry
private[deconstruct] object ConstantPoolEntry {
  case class Utf8(dataIndex: Int, length: Int) extends ConstantPoolEntry
  case class I32(n: Int) extends ConstantPoolEntry
  case class F32(n: Float) extends ConstantPoolEntry
  case class I64(n: Long) extends ConstantPoolEntry
  case class F64(n: Double) extends ConstantPoolEntry
  case class ClassRef(index: Int) extends ConstantPoolEntry
  case class StringRef(index: Int) extends ConstantPoolEntry
  case class FieldRef(classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
  case class MethodRef(classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
  case class InterfaceMethodRef(classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
  case class NameAndType(nameIndex: Int, typeIndex: Int) extends ConstantPoolEntry
}

/**
 * In java class files, the "constant pool" is where almost all string data is stored. Most other
 * data types are indirect pointers into this pool.
 */
private[deconstruct] class ConstantPool(count: Int, data: ByteBuffer) {
  import ConstantPoolEntry._

  private[this] val pool = new Array[ConstantPoolEntry](count)
  private[this] var index = 1

  // for the superclass, a 0 index means "nothing", so seed it with a blank string:
  pool(0) = Utf8(0, 0)

  def apply(n: Int) = pool(n)

  def add(entry: ConstantPoolEntry, advance: Int = 1) {
    pool(index) = entry
    index += advance
  }

  def full = (index == count)

  def stringify(entry: ConstantPoolEntry): String = entry match {
    case Utf8(d, l) => {
      // FIXME: maddeningly, "Utf8" uses a modified form of utf-8 that java can't actually decode.
      // i've decided that for now i don't care.
      if (l > 0) {
        val buffer = new Array[Byte](l)
        data.position(d)
        data.get(buffer)
        new String(buffer, "UTF-8")
      } else {
        ""
      }
    }
    case I32(n) => n.toString
    case F32(n) => n.toString
    case I64(n) => n.toString
    case F64(n) => n.toString
    case ClassRef(i) => stringify(pool(i))
    case StringRef(i) => stringify(pool(i))
/*
    -- interesting! these appear to be unused in reality.
    case FieldRef(c, n) =>
    case MethodRef(c, n) =>
    case InterfaceMethodRef(c, n) =>
    case NameAndType(n, t) =>
*/
  }
}

case class ClassFile(
  pool: ConstantPool,
  majorVersion: Int,
  minorVersion: Int,
  accessFlags: Int,
  classNameEntry: ConstantPoolEntry,
  superclassNameEntry: ConstantPoolEntry,
  interfaceEntries: Seq[ConstantPoolEntry],
  fields: Seq[Deconstruct.Field],
  methods: Seq[Deconstruct.Field],
  attributes: Seq[Deconstruct.Attribute]
) {
  lazy val className = pool.stringify(classNameEntry)
  lazy val superclassName = pool.stringify(superclassNameEntry)
  lazy val interfaces = interfaceEntries.map { pool.stringify(_) }

  def dump: String = {
    "class %s (%d.%d) %s\n".format(className, majorVersion, minorVersion, ClassFlags.toString(accessFlags)) +
    "extends %s\n".format(superclassName) +
    interfaces.map { iface => "with %s\n".format(iface) }.mkString +
    "{\n" +
    fields.map { f => "  field %s: %s %s\n".format(f.name, f.descriptor, FieldFlags.toString(f.accessFlags)) }.mkString +
    methods.map { m => "  method %s: %s %s\n".format(m.name, m.descriptor, MethodFlags.toString(m.accessFlags)) }.mkString +
    attributes.map { a => "  attribute %s: %d bytes\n".format(a.name, a.size) }.mkString +
    "}\n"
  }
}

/**
 * Flags that may be set in the `accessFlags` field of a class.
 */
object ClassFlags {
  val ACC_PUBLIC = 0x0001
  val ACC_FINAL = 0x0010
  val ACC_SUPER = 0x0020
  val ACC_INTERFACE = 0x0200
  val ACC_ABSTRACT = 0x0400
  val ACC_SYNTHETIC = 0x1000
  val ACC_ANNOTATION = 0x2000
  val ACC_ENUM = 0x4000

  def toString(n: Int) = {
    val out = new mutable.ListBuffer[String]
    if ((n & ACC_PUBLIC) != 0) out += "public"
    if ((n & ACC_FINAL) != 0) out += "final"
    if ((n & ACC_SUPER) != 0) out += "super"
    if ((n & ACC_INTERFACE) != 0) out += "interface"
    if ((n & ACC_ABSTRACT) != 0) out += "abstract"
    if ((n & ACC_SYNTHETIC) != 0) out += "synthetic"
    if ((n & ACC_ANNOTATION) != 0) out += "annotation"
    if ((n & ACC_ENUM) != 0) out += "enum"
    out.mkString(",")
  }
}

/**
 * Flags that may be set in the `accessFlags` field of a field.
 */
object FieldFlags {
  val ACC_PUBLIC = 0x0001
  val ACC_PRIVATE = 0x0002
  val ACC_PROTECTED = 0x0004
  val ACC_STATIC = 0x0008
  val ACC_FINAL = 0x0010
  val ACC_VOLATILE = 0x0040
  val ACC_TRANSIENT = 0x0080
  val ACC_SYNTHETIC = 0x1000
  val ACC_ENUM = 0x4000

  def toString(n: Int) = {
    val out = new mutable.ListBuffer[String]
    if ((n & ACC_PUBLIC) != 0) out += "public"
    if ((n & ACC_PRIVATE) != 0) out += "private"
    if ((n & ACC_PROTECTED) != 0) out += "protected"
    if ((n & ACC_STATIC) != 0) out += "static"
    if ((n & ACC_FINAL) != 0) out += "final"
    if ((n & ACC_VOLATILE) != 0) out += "volatile"
    if ((n & ACC_TRANSIENT) != 0) out += "transient"
    if ((n & ACC_SYNTHETIC) != 0) out += "synthetic"
    if ((n & ACC_ENUM) != 0) out += "enum"
    out.mkString(",")
  }
}

/**
 * Flags that may be set in the `accessFlags` field of a method.
 */
object MethodFlags {
  val ACC_PUBLIC = 0x0001
  val ACC_PRIVATE = 0x0002
  val ACC_PROTECTED = 0x0004
  val ACC_STATIC = 0x0008
  val ACC_FINAL = 0x0010
  val ACC_SYNCHRONIZED = 0x0020
  val ACC_BRIDGE = 0x0040
  val ACC_VARARGS = 0x0080
  val ACC_NATIVE = 0x0100
  val ACC_ABSTRACT = 0x0400
  val ACC_STRICT = 0x0800
  val ACC_SYNTHETIC = 0x1000

  def toString(n: Int) = {
    val out = new mutable.ListBuffer[String]
    if ((n & ACC_PUBLIC) != 0) out += "public"
    if ((n & ACC_PRIVATE) != 0) out += "private"
    if ((n & ACC_PROTECTED) != 0) out += "protected"
    if ((n & ACC_STATIC) != 0) out += "static"
    if ((n & ACC_FINAL) != 0) out += "final"
    if ((n & ACC_SYNCHRONIZED) != 0) out += "synchronized"
    if ((n & ACC_BRIDGE) != 0) out += "bridge"
    if ((n & ACC_VARARGS) != 0) out += "varargs"
    if ((n & ACC_NATIVE) != 0) out += "native"
    if ((n & ACC_ABSTRACT) != 0) out += "abstract"
    if ((n & ACC_STRICT) != 0) out += "strict"
    if ((n & ACC_SYNTHETIC) != 0) out += "synthetic"
    out.mkString(",")
  }
}

/**
 * Tool for extracting basic data out of a java class file. This does not bother parsing bytecode
 * or annotations, but instead focuses on the public API of a class.
 *
 * Strings are extracted from the data lazily, on demand, so the initial scan should require only
 * as much time as it takes to parse the data segments.
 */
object Deconstruct {
  case class Attribute(pool: ConstantPool, nameEntry: ConstantPoolEntry, dataIndex: Int, size: Int) {
    lazy val name = pool.stringify(nameEntry)
  }

  case class Field(
    pool: ConstantPool,
    nameEntry: ConstantPoolEntry,
    descriptorEntry: ConstantPoolEntry,
    accessFlags: Int,
    attributes: Seq[Attribute]
  ) {
    lazy val name = pool.stringify(nameEntry)
    lazy val descriptor = pool.stringify(descriptorEntry)
  }

  /**
   * Convert an input stream into a `ByteBuffer` and then extract the class description from it.
   */
  def apply(in: InputStream): ClassFile = {
    val out = new ByteArrayOutputStream()
    val buf = new Array[Byte](1 << 20)
    var n = 0
    do {
      n = in.read(buf, 0, buf.size)
      if (n > 0) out.write(buf, 0, n)
    } while (n >= 0)
    Deconstruct(out.toByteArray)
  }

  /**
   * Convert a byte array into a `ByteBuffer` and then extract the class description from it.
   */
  def apply(data: Array[Byte]): ClassFile = apply(ByteBuffer.wrap(data))

  /**
   * Extract the class description from a `ByteBuffer` containing a java class file.
   */
  def apply(data: ByteBuffer): ClassFile = {
    new Deconstruct(data).scan()
  }
}

private class Deconstruct(data: ByteBuffer) {
  import Deconstruct._
  import ConstantPoolEntry._

  private[this] final val CafeBabe = 0xcafebabe

  def scan(): ClassFile = {
    data.order(ByteOrder.BIG_ENDIAN)
    data.position(0)
    if (data.getInt != CafeBabe) {
      throw new ParseException("Not a class file")
    }

    val minor = data.getShort
    val major = data.getShort
    val pool = new ConstantPool(data.getShort, data)
    scanConstantPool(pool)

    val accessFlags = data.getShort
    val classNameEntry = pool(data.getShort)
    val superclassNameEntry = pool(data.getShort)

    val interfaceIndexes = (0 until data.getShort) map { _ => data.getShort }
    val fields = (0 until data.getShort) map { _ => scanField(pool) }
    val methods = (0 until data.getShort) map { _ => scanField(pool) }
    val attributes = (0 until data.getShort) map { _ => scanAttribute(pool) }

    ClassFile(
      pool,
      major,
      minor,
      accessFlags,
      classNameEntry,
      superclassNameEntry,
      interfaceIndexes.map { pool(_) },
      fields,
      methods,
      attributes
    )
  }

  private[this] def scanField(pool: ConstantPool) = {
    val accessFlags = data.getShort
    val nameIndex = data.getShort
    val descriptorIndex = data.getShort
    val attributeCount = data.getShort
    val attributes = (0 until attributeCount) map { _ => scanAttribute(pool) }
    Field(pool, pool(nameIndex), pool(descriptorIndex), accessFlags, attributes)
  }

  private[this] def scanAttribute(pool: ConstantPool) = {
    val nameIndex = data.getShort
    val size = data.getInt
    val rv = Attribute(pool, pool(nameIndex), data.position, size)
    data.position(data.position + size)
    rv
  }

  private[this] def scanConstantPool(pool: ConstantPool) {
    while (!pool.full) { // we have some bad news about the pool.
      data.get match {
        case 1 => {
          val length = data.getShort
          pool.add(Utf8(data.position, length))
          data.position(data.position + length)
        }
        case 3 => pool.add(I32(data.getInt))
        case 4 => pool.add(F32(data.getFloat))
        case 5 => pool.add(I64(data.getLong), 2)
        case 6 => pool.add(F64(data.getDouble), 2)
        case 7 => pool.add(ClassRef(data.getShort))
        case 8 => pool.add(StringRef(data.getShort))
        case 9 => pool.add(FieldRef(data.getShort, data.getShort))
        case 10 => pool.add(MethodRef(data.getShort, data.getShort))
        case 11 => pool.add(InterfaceMethodRef(data.getShort, data.getShort))
        case 12 => pool.add(NameAndType(data.getShort, data.getShort))
        case n => {
          throw new ParseException("Unknown constant tag " + n + " at position " + data.position)
        }
      }
    }
  }
}
