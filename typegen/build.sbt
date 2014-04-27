name := "typegen"

version := "0.0.1"

organization := "io.llambda"

scalaVersion := "2.11.0"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.4" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
