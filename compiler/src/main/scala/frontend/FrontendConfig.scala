package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

case class FrontendConfig(
  includePath : IncludePath,
  featureIdentifiers : Set[String]
)
