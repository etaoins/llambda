package io.llambda.compiler
import io.llambda

import scala.io.Source
import java.io.FileNotFoundException

class RequiredRuntimeBuildFileNotFoundException(path : String) extends
  Exception(s"Required runtime build file ${path} not found. " +
            s"Ensure that the runtime is built in the ${RuntimeBuildFiles.buildDirectory} path.")

object RuntimeBuildFiles {
  val buildDirectory = "build/"

  val clangFlagsPath = s"${buildDirectory}required-clang-flags"
  val llvmTargetPath = s"${buildDirectory}llvm-target"
  val llcorePath = s"${buildDirectory}libllcore.a"

  private def requiredBuildFile(path : String) = {
    try {
      Source.fromFile(path).mkString
    }
    catch {
      case _ : FileNotFoundException => throw new RequiredRuntimeBuildFileNotFoundException(path)
    }
  }

  lazy val clangFlags = requiredBuildFile(clangFlagsPath)
  lazy val llvmTarget = requiredBuildFile(llvmTargetPath)
}
