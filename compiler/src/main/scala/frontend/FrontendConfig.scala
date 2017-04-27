package io.llambda.compiler.frontend


case class FrontendConfig(
  includePath: IncludePath,
  featureIdentifiers: Set[String],
  traceMacroExpansion: Boolean = false
)
