name := "compiler"

version := "0.0.1"

organization := "io.llambda"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

libraryDependencies += "jline" % "jline" % "2.12.1"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"

val schemeSourceDir = new File("compiler/src/main/scheme")

unmanagedResourceDirectories in Compile += schemeSourceDir

unmanagedResourceDirectories in Test ++= Seq(new File("compiler/src/test/scheme"), schemeSourceDir)
