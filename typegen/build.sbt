name := "typegen"

version := "0.0.1"

organization := "io.llambda"

scalaVersion := "2.12.3"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
