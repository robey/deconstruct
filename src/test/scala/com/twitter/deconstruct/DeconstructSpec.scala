package com.twitter.deconstruct

import org.scalatest._

class DeconstructSpec extends FunSpec {
  val classFile = getClass.getClassLoader.getResourceAsStream("JournalFile.class")

  describe("Deconstruct") {
    it("passes") {
      val c = Deconstruct(classFile)
      println(c.dump)
    }
  }
}
