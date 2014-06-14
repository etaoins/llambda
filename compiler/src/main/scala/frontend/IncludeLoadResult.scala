package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.ast

private[frontend] case class IncludeLoadResult(
  filename : String,
  innerIncludePath : IncludePath,
  data : List[ast.Datum]
)
