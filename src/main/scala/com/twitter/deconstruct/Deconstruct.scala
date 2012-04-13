package com.twitter.deconstruct

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

class ParseException(reason: String) extends Exception(reason)

sealed trait ConstantPoolEntry
object ConstantPoolEntry {
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

case class ClassFile(
  private val d: Deconstruct,
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
  def className = d.stringify(classNameEntry)
  def superclassName = d.stringify(superclassNameEntry)
  def interfaces = interfaceEntries.map { d.stringify(_) }

  def dump: String = {
    "class %s (%d.%d) %s\n".format(className, majorVersion, minorVersion, ClassAttributes.toString(accessFlags)) +
    "extends %s\n".format(superclassName) +
    interfaces.map { iface => "with %s\n".format(iface) }.mkString +
    "{\n" +
    fields.map { f => "  field %s: %s %s\n".format(f.name, f.descriptor, FieldAttributes.toString(f.accessFlags)) }.mkString +
    methods.map { m => "  method %s: %s %s\n".format(m.name, m.descriptor, MethodAttributes.toString(m.accessFlags)) }.mkString +
    attributes.map { a => "  attribute %s: %d bytes\n".format(a.name, a.size) }.mkString +
    "}\n"
  }
}

object ClassAttributes {
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

object FieldAttributes {
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

object MethodAttributes {
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

object Deconstruct {
  case class Attribute(private val d: Deconstruct, nameEntry: ConstantPoolEntry, dataIndex: Int, size: Int) {
    def name = d.stringify(nameEntry)
  }

  case class Field(
    private val d: Deconstruct,
    nameEntry: ConstantPoolEntry,
    descriptorEntry: ConstantPoolEntry,
    accessFlags: Int,
    attributes: Seq[Attribute]
  ) {
    def name = d.stringify(nameEntry)
    def descriptor = d.stringify(descriptorEntry)
  }

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

  def apply(data: Array[Byte]): ClassFile = apply(ByteBuffer.wrap(data))

  def apply(data: ByteBuffer): ClassFile = {
    new Deconstruct(data).scan()
  }
}

class Deconstruct(data: ByteBuffer) {
  import Deconstruct._
  import ConstantPoolEntry._

  private[this] final val CafeBabe = 0xcafebabe
  final val constantPool = new mutable.HashMap[Int, ConstantPoolEntry]

  // for the superclass, a 0 index means "nothing", so seed it with a blank string:
  constantPool(0) = Utf8(0, 0)

  def scan(): ClassFile = {
    data.order(ByteOrder.BIG_ENDIAN)
    data.position(0)
    if (data.getInt != CafeBabe) {
      throw new ParseException("Not a class file")
    }

    val minor = data.getShort
    val major = data.getShort
    scanConstantPool(data.getShort)

    val accessFlags = data.getShort
    val classNameEntry = constantPool(data.getShort)
    val superclassNameEntry = constantPool(data.getShort)

    val interfaceIndexes = (0 until data.getShort) map { _ => data.getShort }
    val fields = (0 until data.getShort) map { _ => scanField() }
    val methods = (0 until data.getShort) map { _ => scanField() }
    val attributes = (0 until data.getShort) map { _ => scanAttribute() }

    ClassFile(
      this,
      major,
      minor,
      accessFlags,
      classNameEntry,
      superclassNameEntry,
      interfaceIndexes.map { constantPool(_) },
      fields,
      methods,
      attributes
    )
  }

  private[this] def scanField() = {
    val accessFlags = data.getShort
    val nameIndex = data.getShort
    val descriptorIndex = data.getShort
    val attributeCount = data.getShort
    val attributes = (0 until attributeCount) map { _ => scanAttribute() }
    Field(this, constantPool(nameIndex), constantPool(descriptorIndex), accessFlags, attributes)
  }

  private[this] def scanAttribute() = {
    val nameIndex = data.getShort
    val size = data.getInt
    val rv = Attribute(this, constantPool(nameIndex), data.position, size)
    data.position(data.position + size)
    rv
  }

  private[this] def scanConstantPool(count: Int) {
    var constantIndex = 1
    while (constantIndex < count) {
      val entry = data.get match {
        case 1 => {
          val length = data.getShort
          constantPool(constantIndex) = Utf8(data.position, length)
          data.position(data.position + length)
          constantIndex += 1
        }
        case 3 => {
          constantPool(constantIndex) = I32(data.getInt)
          constantIndex += 1
        }
        case 4 => {
          constantPool(constantIndex) = F32(data.getFloat)
          constantIndex += 1
        }
        case 5 => {
          constantPool(constantIndex) = I64(data.getLong)
          constantIndex += 2
        }
        case 6 => {
          constantPool(constantIndex) = F64(data.getDouble)
          constantIndex += 2
        }
        case 7 => {
          constantPool(constantIndex) = ClassRef(data.getShort)
          constantIndex += 1
        }
        case 8 => {
          constantPool(constantIndex) = StringRef(data.getShort)
          constantIndex += 1
        }
        case 9 => {
          constantPool(constantIndex) = FieldRef(data.getShort, data.getShort)
          constantIndex += 1
        }
        case 10 => {
          constantPool(constantIndex) = MethodRef(data.getShort, data.getShort)
          constantIndex += 1
        }
        case 11 => {
          constantPool(constantIndex) = InterfaceMethodRef(data.getShort, data.getShort)
          constantIndex += 1
        }
        case 12 => {
          constantPool(constantIndex) = NameAndType(data.getShort, data.getShort)
          constantIndex += 1
        }
        case n => throw new ParseException("Unknown constant pool tag " + n + " at position " + data.position)
      }
    }
  }

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
    case ClassRef(i) => stringify(constantPool(i))
    case StringRef(i) => stringify(constantPool(i))
    case FieldRef(c, n) => stringify(constantPool(c)) + "/" + stringify(constantPool(n))
/*  case class MethodRef(classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
  case class InterfaceMethodRef(classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
  case class NameAndType(nameIndex: Int, typeIndex: Int) extends ConstantPoolEntry
  */
  }
}
