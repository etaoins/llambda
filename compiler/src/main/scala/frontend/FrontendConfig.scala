package io.llambda.compiler.frontend
import io.llambda

case class FrontendConfig(
  includePath : IncludePath,
  featureIdentifiers : Set[String],
  traceMacroExpansion : Boolean = false
)
