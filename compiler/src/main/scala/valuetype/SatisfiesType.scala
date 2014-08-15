package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}

object SatisfiesType {
  private def unionLikeSatisfiesType(
      superType : SchemeType,
      testingMemberTypes : Set[NonUnionSchemeType]
  ) : Option[Boolean] = {
    val memberResult = testingMemberTypes.map(apply(superType, _)) 

    if (memberResult == Set(Some(true))) {
      // All member types satisfy this type
      Some(true)
    }
    else if (memberResult == Set(Some(false))) {
      // No member types satisfy this type
      Some(false)
    }
    else {
      None
    }
  }

  /** Determines if a Scheme type satisfies another
    *
    * Some(true) indicates all values of test testing type satisfy the super type. Some(false) indicates no values of
    * the testing type satisfy the super type.  None indicates that some values of the testing type satisfy the super
    * type.
    */
  def apply(superType : SchemeType, testingType : SchemeType) : Option[Boolean] = {
    (superType, testingType) match {
      case (superAny, _) if superAny eq AnySchemeType =>
        // Quick optimisation - this should not affect correctness
        Some(true)
      
      case (_, EmptySchemeType) =>
        // The empty type satisfies all types
        Some(true)

      case (EmptySchemeType, _) =>
        // Nothing satisfies the empty type except itself
        Some(false)

      case (superList @ ProperListType(superMember), _) =>
        testingType match {
          case ProperListType(testingMember) =>
            if (SatisfiesType(superMember, testingMember) == Some(true)) {
              Some(true)
            }
            else {
              // This makes me unhappy
              // Two "typed" proper lists can satisfy each other if they're both empty
              None
            }

          case testingPair : PairType =>
            for(carSatisfies <- SatisfiesType(superMember, testingPair.carType);
                cdrSatisfies <- SatisfiesType(superList, testingPair.cdrType))
            yield
              (carSatisfies && cdrSatisfies)

          case EmptyListType =>
            // Empty list satisfies all proper lists
            Some(true)

          case UnionType(testingMemberTypes) =>
            unionLikeSatisfiesType(superList, testingMemberTypes)

          case _ =>
            Some(false)
        }

      case (_, testingList @ ProperListType(testingMember)) =>
        // Treat this as a special kind of union
        val testingMemberTypes = Set[NonUnionSchemeType](
          EmptyListType,
          SpecificPairType(testingMember, testingList)
        )

        unionLikeSatisfiesType(superType, testingMemberTypes)

      case (_, UnionType(testingMemberTypes)) =>
        unionLikeSatisfiesType(superType, testingMemberTypes)

      case (UnionType(memberTypes), _) =>
        val memberResult = memberTypes.map(apply(_, testingType))

        if (memberResult.contains(Some(true))) {
          // We satisfy at least one member type
          Some(true)
        }
        else if (memberResult == Set(Some(false))) {
          // We satisfy no member types
          Some(false)
        }
        else {
          None
        }

      case (superAtom : SchemeTypeAtom, testingAtom : SchemeTypeAtom) =>
        // Atoms are easy
        Some(superAtom == testingAtom)

      case (superValue : ConstantValueType, testingValue : ConstantValueType) =>
        Some(superValue == testingValue)

      case (superRecord : RecordType, testingRecord : RecordType) =>
        // Record types satisfy themselves
        Some(superRecord eq testingRecord)
      
      case (superPair : PairType, testingPair : PairType) =>
        // Pairs satisfy their more general pairs
        val memberResults = Set(
          SatisfiesType(superPair.carType, testingPair.carType),
          SatisfiesType(superPair.cdrType, testingPair.cdrType)
        )

        if (memberResults.contains(Some(false))) {
          // Definitely not compatible
          Some(false)
        }
        else if (memberResults.contains(None)) {
          // May satisfy
          None
        }
        else {
          // Definitely satisfies
          Some(true)
        }

      case (superDerived : DerivedSchemeType, _) =>
        // Didn't have an exact match - check our parent types
        apply(superDerived.parentType, testingType) match {
          case Some(false) =>
            // Definitely doesn't match
            Some(false)

          case _ =>
            // May match a super type
            None
        }
      
      case (_, testingDerived : DerivedSchemeType) =>
        apply(superType, testingDerived.parentType)
    }
  }
}
