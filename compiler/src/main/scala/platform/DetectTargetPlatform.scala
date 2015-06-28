package io.llambda.compiler.platform
import io.llambda

import llambda.compiler.RuntimeBuildFiles

class TargetDetectionFailureException extends Exception("Unable to automatically detect target platform")

object DetectTargetPlatform {
  private lazy val targetPlatform = {
    DetectLlvmTarget.fromLlvmIr(RuntimeBuildFiles.llvmTarget) getOrElse {
      Console.err.println("Unable to detect target platform from LLVM triple; falling back to JVM host detection")

      DetectJvmPlatform() getOrElse {
        throw new TargetDetectionFailureException
      }
    }
  }

  def apply() : TargetPlatform = targetPlatform
}
