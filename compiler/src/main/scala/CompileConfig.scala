package io.llambda.compiler
import io.llambda

import llambda.compiler.frontend.{IncludePath, IncludeLoader}

case class CompileConfig(
  includePath : IncludePath,
  optimizeLevel : Int = 0,
  emitLlvm : Boolean = false,
  targetPlatform : platform.TargetPlatform
)
