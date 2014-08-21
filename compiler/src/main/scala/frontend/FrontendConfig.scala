package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.dialect

case class FrontendConfig(
  includePath : IncludePath,
  featureIdentifiers : Set[String],
  schemeDialect : dialect.Dialect
)
