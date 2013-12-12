name := "typegen"

version := "0.0.1"

organization := "io.llambda"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")
