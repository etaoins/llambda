package llambda

import llambda.frontend.{IncludePath, IncludeLoader}

case class CompileConfig(
  includePath : IncludePath,
  optimizeLevel : Int = 0,
  emitLlvm : Boolean = false
)
