package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.dialect
import llambda.compiler.{celltype => ct}

object StabiliseType {
  /** Recursively stablises a pair elements' types and replaces its applicable type
    *
    * Pairs can be cast to AnyPair at any time which has an applicable type of TopProcedureType. This means we always
    * construct pairs containing the top procedure type, even in dialects with immutable types. This ensures that the
    * stablised type reflects that.
    */
  private def replacePairElementRef(typeRef : SchemeTypeRef, schemeDialect : dialect.Dialect) : SchemeTypeRef =
    typeRef match {
      case DirectSchemeTypeRef(directType) =>
        // Pairs are always constructed with the top procedure type
        DirectSchemeTypeRef(apply(directType, schemeDialect).replaceApplicableType(TopProcedureType))

      case other =>
        other
    }

  /** Returns the part of a Scheme type that can't be mutated at runtime
    *
    * For example, if pairs are mutable then it's not possible for a storage location to have a stable specific pair
    * type. This function will convert any pairs encountered to the <pair> top type.
    */
  def apply(schemeType : SchemeType, schemeDialect : dialect.Dialect) : SchemeType = schemeType match {
    case UnionType(memberTypes) =>
      val replacedMembers = memberTypes.map(apply(_, schemeDialect))
      SchemeType.fromTypeUnion(replacedMembers)

    case pairType : SpecificPairType =>
      if (schemeDialect.pairsAreImmutable) {
        SpecificPairType(
          replacePairElementRef(pairType.carTypeRef, schemeDialect),
          replacePairElementRef(pairType.cdrTypeRef, schemeDialect)
        )
      }
      else {
        AnyPairType
      }

    case other =>
      other
  }
}
