name := "compiler"

version := "0.0.1"

organization := "io.llambda"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.0" % "test"

libraryDependencies += "org.scala-lang" % "jline" % "2.10.3"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

val schemeSourceDir = new File("compiler/src/main/scheme")

unmanagedResourceDirectories in Compile += schemeSourceDir

unmanagedResourceDirectories in Test ++= Seq(new File("compiler/src/test/scheme"), schemeSourceDir)
