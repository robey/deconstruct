import com.twitter.sbt._

import com.twitter.scalatest._

seq((
  Project.defaultSettings ++
  StandardProject.newSettings ++
  SubversionPublisher.newSettings ++
  ScalaTestMixins.testSettings
): _*)

organization := "com.twitter"

name := "deconstruct"

version := "1.0.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scala-lang" % "jline" % "2.9.1",
  "org.scalatest" %% "scalatest" % "1.7.1" % "test",
  "com.twitter" %% "scalatest-mixins" % "1.0.2" % "test"
)

