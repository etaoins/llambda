name := "compiler"

version := "0.0.1"

organization := "io.llambda"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test"

libraryDependencies += "jline" % "jline" % "2.11"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

val schemeSourceDir = new File("compiler/src/main/scheme")

unmanagedResourceDirectories in Compile += schemeSourceDir

unmanagedResourceDirectories in Test ++= Seq(new File("compiler/src/test/scheme"), schemeSourceDir)
