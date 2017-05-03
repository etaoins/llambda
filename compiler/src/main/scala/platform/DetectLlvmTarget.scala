package io.llambda.compiler.platform
import io.llambda

import llambda.llvmir.DataLayout
import llambda.compiler.RuntimeBuildFiles


class DetectLlvmTargetFailureException extends Exception("Unable to parse LLVM target platform")

object DetectLlvmTarget {
  def apply(): TargetPlatform = fromLlvmIr(RuntimeBuildFiles.llvmTarget)

  def fromLlvmIr(llvmir: String): TargetPlatform = {
    val DataLayoutString = """(?m)^\s*target\s+datalayout\s*=\s*"([^"]*)"\s*$""".r

    DataLayoutString.findFirstMatchIn(llvmir) map { m =>
      TargetPlatform(DataLayout.fromDataLayoutString(m.group(1)))
    } getOrElse {
      throw new DetectLlvmTargetFailureException
    }
  }
}
