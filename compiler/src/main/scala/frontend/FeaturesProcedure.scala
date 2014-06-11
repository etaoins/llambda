package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

private[frontend] object FeaturesProcedure {
  def apply()(implicit frontendConfig : FrontendConfig) : et.Lambda = {
    // Create a fake lambda for this
    // This prevents us from having to duplicate features between the frontend and stdlib
    val featuresList = frontendConfig.featureIdentifiers.toList.sorted

    var featuresProc = et.Lambda(
      Nil,
      None,
      et.Literal(ast.ProperList(featuresList.map(ast.Symbol(_))))
    )

    featuresProc
  }
}
