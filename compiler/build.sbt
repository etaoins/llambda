name := "compiler"

version := "0.0.1"

organization := "io.llambda"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "jline" % "jline" % "2.11"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.0.1"

val schemeSourceDir = new File("compiler/src/main/scheme")

unmanagedResourceDirectories in Compile += schemeSourceDir

unmanagedResourceDirectories in Test ++= Seq(new File("compiler/src/test/scheme"), schemeSourceDir)
