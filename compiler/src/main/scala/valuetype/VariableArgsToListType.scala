package io.llambda.compiler.valuetype
import io.llambda

object VariableArgsToListType {
  /** Converts a list of optional args an a rest argument member type in to a list type  */
  def apply(optionalArgs : List[SchemeType], restArgMemberTypeOpt : Option[SchemeType]) : SchemeType = {
    val restArgListType = restArgMemberTypeOpt match {
      case None =>
        EmptyListType

      case Some(memberType) =>
        UniformProperListType(memberType)
    }

    optionalArgs.foldRight(restArgListType) { (optionalArgType, cdrType) =>
      UnionType(Set(
        SpecificPairType(
          DirectSchemeTypeRef(optionalArgType),
          DirectSchemeTypeRef(cdrType)
        ),
        EmptyListType
      ))
    }
  }
}
