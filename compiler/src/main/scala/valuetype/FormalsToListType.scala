package io.llambda.compiler.valuetype


object FormalsToListType {
  /** Converts a formals definition in to a list type that matches the formal's expected args */
  def apply(
      mandatoryArgs: List[SchemeType],
      optionalArgs: List[SchemeType],
      restArgMemberTypeOpt: Option[SchemeType]
  ): SchemeType = {
    val varArgsListType = VariableArgsToListType(optionalArgs, restArgMemberTypeOpt)

    mandatoryArgs.foldRight(varArgsListType) { case (mandatoryArgType, cdrType) =>
      SpecificPairType(
        DirectSchemeTypeRef(mandatoryArgType),
        DirectSchemeTypeRef(cdrType)
      )
    }
  }
}
