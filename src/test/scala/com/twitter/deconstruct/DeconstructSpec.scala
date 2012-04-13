package com.twitter.deconstruct

import org.scalatest._

class DeconstructSpec extends FunSpec {

  describe("Deconstruct") {
    it("JournalFile.class") {
      val classfile = Deconstruct(getClass.getClassLoader.getResourceAsStream("JournalFile.class"))

      assert((classfile.accessFlags & ClassAttributes.ACC_PUBLIC) != 0)
      assert((classfile.accessFlags & ClassAttributes.ACC_SYNTHETIC) === 0)
      assert(classfile.className === "com/twitter/libkestrel/JournalFile")
      assert(classfile.superclassName === "java/lang/Object")
      assert(classfile.interfaces.size === 0)
      assert(classfile.majorVersion === 49)
      assert(classfile.minorVersion === 0)
      assert(classfile.attributes.size === 3)

      assert(classfile.fields.size === 0)
      assert(classfile.methods.size === 4)

      assert(classfile.methods(0).name === "open")
      assert((classfile.methods(0).accessFlags & MethodAttributes.ACC_PUBLIC) != 0)
      assert((classfile.methods(0).accessFlags & MethodAttributes.ACC_SYNTHETIC) === 0)
      assert(classfile.methods(0).descriptor === "(Ljava/io/File;)Lcom/twitter/libkestrel/JournalFileReader;")

    }

    it("JournaledQueue.class") {
      val classfile = Deconstruct(getClass.getClassLoader.getResourceAsStream("JournaledQueue.class"))

      assert(classfile.className === "com/twitter/libkestrel/JournaledQueue")
      assert(classfile.interfaces.size === 2)
      assert(classfile.interfaces(0) === "com/twitter/concurrent/Serialized")
      assert(classfile.interfaces(1) === "scala/ScalaObject")
      assert(classfile.attributes.size === 4)

      assert(classfile.fields.size === 11)
      assert(classfile.methods.size === 26)
      assert(classfile.methods(8).name == "items")
      assert(classfile.methods(8).descriptor == "()J")
    }

    it("JournaledQueue$Reader.class") {
      val classfile = Deconstruct(getClass.getClassLoader.getResourceAsStream("JournaledQueue$Reader.class"))

      assert(classfile.className === "com/twitter/libkestrel/JournaledQueue$Reader")
      assert(classfile.interfaces.size === 2)
      assert(classfile.attributes.size === 3)

      assert(classfile.fields.size === 23)
      assert(classfile.fields(1).name === "readerConfig")
      assert(classfile.fields(1).accessFlags === (FieldAttributes.ACC_PRIVATE | FieldAttributes.ACC_FINAL))
      assert(classfile.fields(1).descriptor === "Lcom/twitter/libkestrel/config/JournaledQueueReaderConfig;")

      assert(classfile.methods.size === 61)
      assert(classfile.methods(25).name == "openItems")
      assert(classfile.methods(25).accessFlags == MethodAttributes.ACC_PUBLIC)
      assert(classfile.methods(25).descriptor == "()I")
    }
  }
}
