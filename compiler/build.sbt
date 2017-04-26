name := "compiler"

version := "0.0.1"

organization := "io.llambda"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"

libraryDependencies += "jline" % "jline" % "2.14.2"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.3"

val schemeSourceDir = new File("compiler/src/main/scheme")

unmanagedResourceDirectories in Compile += schemeSourceDir

unmanagedResourceDirectories in Test ++= Seq(new File("compiler/src/test/scheme"), schemeSourceDir)
