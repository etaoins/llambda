package io.llambda.compiler.valuetype


object StabiliseType {
  /** Recursively stablises a pair elements' types and replaces its procedure type
    *
    * Pairs can be cast to AnyPair at any time which has an procedure type of TopProcedureType. This means we always
    * construct pairs containing the top procedure type. This ensures that the stablised type reflects that.
    */
  private def replacePairElementRef(typeRef: SchemeTypeRef): SchemeTypeRef =
    typeRef match {
      case DirectSchemeTypeRef(directType) =>
        // Pairs are always constructed with the top procedure type
        DirectSchemeTypeRef(apply(directType).replaceProcedureType(TopProcedureType))

      case other =>
        other
    }

  /** Returns the part of a Scheme type that can't be mutated at runtime
    *
    * For example, if pairs are mutable then it's not possible for a storage location to have a stable specific pair
    * type. This function will convert any pairs encountered to the <pair> top type.
    */
  def apply(schemeType: SchemeType): SchemeType = schemeType match {
    case UnionType(memberTypes) =>
      val replacedMembers = memberTypes.map(apply)
      SchemeType.fromTypeUnion(replacedMembers)

    case pairType: SpecificPairType =>
      SpecificPairType(
        replacePairElementRef(pairType.carTypeRef),
        replacePairElementRef(pairType.cdrTypeRef)
      )

    case other =>
      other
  }
}
