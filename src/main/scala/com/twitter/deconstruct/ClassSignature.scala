package com.twitter.deconstruct

case class FieldSignature(name: String, descriptor: String, flags: Int) {
  def signature = "%s(%08x):%s".format(name, flags, descriptor)

  // must be an exact match.
  def isCompatibleWith(old: FieldSignature): Boolean = {
    name == old.name && descriptor == old.descriptor && flags == old.flags
  }
}

case class ClassSignature(
  name: String,
  flags: Int,
  superclass: String,
  interfaces: Seq[String],
  fields: Seq[FieldSignature],
  methods: Seq[FieldSignature]
) {
  def signature = {
    "%s(%08x):%s:%s\n".format(name, flags, superclass, interfaces.mkString("{", ":", "}")) +
      fields.map { f => "F:%s\n".format(f.signature) }.mkString +
      methods.map { m => "M:%s\n".format(m.signature) }.mkString
  }

  def isCompatibleWith(old: ClassSignature): Boolean = {
    name == old.name &&
      flags == old.flags &&
      superclass == old.superclass &&
      interfaces == old.interfaces &&
      old.fields.forall { of => fields.exists { f => f isCompatibleWith of } } &&
      old.methods.forall { om => methods.exists { m => m isCompatibleWith om } }
  }
}

object ClassSignature {
  /**
   * Create a class signature out of a parsed java class file.
   * The signature can be used to check compatibility with previous versions of the same class.
   */
  def apply(classfile: ClassFile): ClassSignature = {
    ClassSignature(
      classfile.className,
      classfile.accessFlags,
      classfile.superclassName,
      classfile.interfaces,
      classfile.fields.map { f => FieldSignature(f.name, f.descriptor, f.accessFlags) },
      classfile.methods.map { m => FieldSignature(m.name, m.descriptor, m.accessFlags) }
    )
  }
}
