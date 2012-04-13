package com.twitter.deconstruct

import org.scalatest._

class ClassSignatureSpec extends FunSpec {
  val classfile = Deconstruct(getClass.getClassLoader.getResourceAsStream("JournalFile.class"))
  // give it at least one field.
  val sig = ClassSignature(classfile).copy(
    fields = Seq(
      FieldSignature("fake", "J", FieldFlags.ACC_PUBLIC),
      FieldSignature("fake2", "J", FieldFlags.ACC_PUBLIC)
    )
  )

  describe("ClassSignature") {
    it("JournalFile.class") {
      val expect = Seq(
        "com/twitter/libkestrel/JournalFile(00000031):java/lang/Object:{}",
        "F:fake(00000001):J",
        "F:fake2(00000001):J",
        "M:open(00000019):(Ljava/io/File;)Lcom/twitter/libkestrel/JournalFileReader;",
        "M:create(00000019):(Ljava/io/File;Ljava/util/concurrent/ScheduledExecutorService;" +
          "Lcom/twitter/util/Duration;Lcom/twitter/util/StorageUnit;)" +
          "Lcom/twitter/libkestrel/JournalFileWriter;",
        "M:append(00000019):(Ljava/io/File;Ljava/util/concurrent/ScheduledExecutorService;" +
          "Lcom/twitter/util/Duration;Lcom/twitter/util/StorageUnit;)" +
          "Lcom/twitter/libkestrel/JournalFileWriter;",
        "M:HEADER_WRITER(00000019):()I"
      )
      assert(sig.signature === expect.mkString("", "\n", "\n"))
    }

    describe("checks compatibility") {
      it("when classes are identical") {
        assert(sig.isCompatibleWith(ClassSignature(classfile)))
      }

      it("when a field is added") {
        val newsig = sig.copy(fields = sig.fields ++ Seq(FieldSignature("hork", "()I", 0)))
        assert(newsig.isCompatibleWith(sig))
      }

      it("when a method is added") {
        val newsig = sig.copy(methods = sig.methods ++ Seq(FieldSignature("hork", "()I", 0)))
        assert(newsig.isCompatibleWith(sig))
      }

      it("not when a field is changed") {
        val newhead = FieldSignature(sig.fields.head.name, sig.fields.head.descriptor, 0)
        val newsig = sig.copy(fields = Seq(newhead) ++ sig.fields.tail)
        assert(!newsig.isCompatibleWith(sig))
      }

      it("not when a method is changed") {
        val newhead = FieldSignature(sig.methods.head.name, sig.methods.head.descriptor, 0)
        val newsig = sig.copy(methods = Seq(newhead) ++ sig.methods.tail)
        assert(!newsig.isCompatibleWith(sig))
      }

      it("not when a field is removed") {
        val newsig = sig.copy(fields = sig.fields.tail)
        assert(!newsig.isCompatibleWith(sig))
      }

      it("not when a method is remoned") {
        val newsig = sig.copy(methods = sig.methods.tail)
        assert(!newsig.isCompatibleWith(sig))
      }
    }
  }
}
