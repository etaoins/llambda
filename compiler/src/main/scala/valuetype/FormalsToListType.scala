package io.llambda.compiler.valuetype
import io.llambda

object FormalsToListType {
  /** Converts a formals definition in to a list type that matches the formal's expected args */
  def apply(fixedArgs : List[SchemeType], restArgMemberTypeOpt : Option[SchemeType]) : SchemeType = {
    val fixedArgCdr = restArgMemberTypeOpt match {
      case None =>
        EmptyListType

      case Some(memberType) =>
        UniformProperListType(memberType)
    }

    fixedArgs.foldRight(fixedArgCdr) { case (fixedArgType, cdrType) =>
      SpecificPairType(
        DirectSchemeTypeRef(fixedArgType),
        DirectSchemeTypeRef(cdrType)
      )
    }
  }
}
