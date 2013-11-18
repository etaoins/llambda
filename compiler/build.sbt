name := "llambda"

version := "0.0.1"

organization := "llambda"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "org.scala-lang" % "jline" % "2.10.3"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")

val schemeSourceDir = new File("src/main/scheme")

unmanagedResourceDirectories in Compile += schemeSourceDir

unmanagedResourceDirectories in Test ++= Seq(new File("src/test/scheme"), schemeSourceDir)
