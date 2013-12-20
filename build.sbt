name := "llambda"

version := "0.0.1"

organization := "io.llambda"

lazy val llvmir = project

lazy val compiler = project.dependsOn(llvmir)

lazy val typegen = project.dependsOn(llvmir)
