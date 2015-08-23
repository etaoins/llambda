package io.llambda.compiler.valuetype
import io.llambda

object FormalsToListType {
  /** Converts a formals definition in to a list type that matches the formal's expected args */
  def apply(
      mandatoryArgs : List[SchemeType],
      optionalArgs : List[SchemeType],
      restArgMemberTypeOpt : Option[SchemeType]
  ) : SchemeType = {
    val restArgListType = restArgMemberTypeOpt match {
      case None =>
        EmptyListType

      case Some(memberType) =>
        UniformProperListType(memberType)
    }

    val restAndOptionalListType = optionalArgs.foldRight(restArgListType) { (optionalArgType, cdrType) =>
      UnionType(Set(
        SpecificPairType(
          DirectSchemeTypeRef(optionalArgType),
          DirectSchemeTypeRef(cdrType)
        ),
        EmptyListType
      ))
    }

    mandatoryArgs.foldRight(restAndOptionalListType) { case (fixedArgType, cdrType) =>
      SpecificPairType(
        DirectSchemeTypeRef(fixedArgType),
        DirectSchemeTypeRef(cdrType)
      )
    }
  }
}
