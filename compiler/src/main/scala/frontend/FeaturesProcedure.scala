package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

import llambda.compiler.valuetype.Implicits._

private[frontend] object FeaturesProcedure {
  def apply()(implicit frontendConfig : FrontendConfig) : et.Lambda = {
    // Create a fake lambda for this
    // This prevents us from having to duplicate features between the frontend and stdlib
    val featuresList = frontendConfig.featureIdentifiers.toList.sorted

    val featuresProcType = vt.ProcedureType(
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UniformProperListType(vt.SymbolType))
    )

    var featuresProc = et.Lambda(
      featuresProcType,
      Nil,
      None,
      et.Literal(ast.ProperList(featuresList.map(ast.Symbol(_))))
    )

    featuresProc
  }
}
