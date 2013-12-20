name := "typegen"

version := "0.0.1"

organization := "io.llambda"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")
