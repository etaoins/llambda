package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

private[frontend] object FeaturesProcedure {
  def apply(frontendConfig: FrontendConfig): et.Lambda = {
    // Create a fake lambda for this
    // This prevents us from having to duplicate features between the frontend and stdlib
    val featuresList = frontendConfig.featureIdentifiers.toList.sorted

    val featuresProcType = vt.ProcedureType(
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UniformProperListType(vt.SymbolType))
    )

    var featuresProc = et.Lambda(
      polyType=featuresProcType.toPolymorphic,
      mandatoryArgs=Nil,
      optionalArgs=Nil,
      restArgOpt=None,
      body=et.Literal(ast.ProperList(featuresList.map(ast.Symbol(_))))
    )

    featuresProc
  }
}
