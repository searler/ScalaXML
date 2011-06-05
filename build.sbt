organization := "cognitiveentity"

name := "ScalaXML"

version := "1.0.2"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
   "org.specs2" %% "specs2" % "1.4" % "test",
   "org.specs2" %% "specs2-scalaz-core" % "6.0.RC2" % "test"
)

testFrameworks += new TestFramework("org.specs2.runner.SpecsFramework")