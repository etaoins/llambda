package io.llambda.compiler.valuetype


private[valuetype] object UnrollType {
  /** Unrolls a recursive type reference
    *
    * @see [[unrollType]]
    */
  def unrollTypeRef(
      schemeTypeRef: SchemeTypeRef,
      replacementType: SchemeType,
      depth: Int
  ): SchemeTypeRef = {
    schemeTypeRef match {
      case RecursiveSchemeTypeRef(`depth`) =>
        // Replacment!
        DirectSchemeTypeRef(replacementType)

      case DirectSchemeTypeRef(directType) =>
        // Replace inside the type reference
        // The fact we're a reference implies that we're a point of indirection for a recursion purposes so increase
        // our depth
        DirectSchemeTypeRef(
          unrollType(directType, replacementType, depth + 1)
        )

      case otherRecursiveRef =>
        otherRecursiveRef
    }
  }

  def unrollNonUnionType(inputType: NonUnionSchemeType, replacementType: SchemeType, depth: Int): NonUnionSchemeType = inputType match {
    case pairType: SpecificPairType =>
      val replacedCar = unrollTypeRef(pairType.carTypeRef, replacementType, depth)
      val replacedCdr = unrollTypeRef(pairType.cdrTypeRef, replacementType, depth)

      SpecificPairType(replacedCar, replacedCdr)

    case other: NonRecursiveType =>
      other
  }

  /** Unrolls a recursive type
    *
    * Unrolling a type involes replacing any recursive reference with a literal copy of the type. Every unrolling
    * expands the type to a larger but equivalent type
    *
    * @param  inputType        Type to unroll recursive types in
    * @param  replacementType  Type ro insert in the specified recursive position. In basic unrolling this is the same
    *                          as the input type but when a type has been modified it may be the original version of
    *                          input type that has now invalid references.
    * @param  depth            Depth of the replacement type inside the input type. In normal unrolling this is 0 but
    *                          when e.g. replacing the union type inside union members this can be > 0
    */
  def unrollType(inputType: SchemeType, replacementType: SchemeType, depth: Int): SchemeType = inputType match {
    case UnionType(memberTypes) =>
      val replacedMembers = memberTypes.map(unrollType(_, replacementType, depth + 1))
      SchemeType.fromTypeUnion(replacedMembers)

    case nonUnion: NonUnionSchemeType =>
      unrollNonUnionType(nonUnion, replacementType, depth)
  }
}
