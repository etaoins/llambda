package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}

object SatisfiesType {
  private def satisfiesTypeRef(
      superTypeRef : SchemeTypeRef,
      superStack : SchemeType.Stack,
      testingTypeRef : SchemeTypeRef,
      testingStack : SchemeType.Stack
  ) : Option[Boolean] = {
    // Different depths - resolve the types and then compare them
    val superType = superTypeRef.resolve(superStack)
    val testingType = testingTypeRef.resolve(testingStack)

    if (superStack.contains(superType) && testingStack.contains(testingType)) {
      Some(true)
    }
    else {
      stackedSatisfiesType(superType :: superStack, testingType :: testingStack)
    }
  }

  def stackedSatisfiesType(superStack : SchemeType.Stack, testingStack : SchemeType.Stack) : Option[Boolean] = {
    (superStack.head, testingStack.head) match {
      case (superAny, _) if superAny eq AnySchemeType =>
        // Quick optimisation - this should not affect correctness
        Some(true)
      
      case (_, EmptySchemeType) =>
        // The empty type satisfies all types
        Some(true)

      case (EmptySchemeType, _) =>
        // Nothing satisfies the empty type except itself
        Some(false)

      case (_, UnionType(testingMemberTypes)) =>
        val memberResults = testingMemberTypes.map { testingMemberType =>
          stackedSatisfiesType(superStack, testingMemberType :: testingStack)
        }

        if (memberResults == Set(Some(true))) {
          // All member types satisfy this type
          Some(true)
        }
        else if (memberResults == Set(Some(false))) {
          // No member types satisfy this type
          Some(false)
        }
        else {
          None
        }

      case (UnionType(superMemberTypes), _) =>
        val memberResults = superMemberTypes.map { superMemberType =>
          val memberResult = stackedSatisfiesType(superMemberType :: superStack, testingStack)

          if (memberResult == Some(true)) {
            // We found at least one type - we can abandon early
            return Some(true)
          }

          memberResult
        }

        if (memberResults == Set(Some(false))) {
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
        val memberResultss = Set(
          satisfiesTypeRef(superPair.carTypeRef, superStack, testingPair.carTypeRef, testingStack),
          satisfiesTypeRef(superPair.cdrTypeRef, superStack, testingPair.cdrTypeRef, testingStack)
        )

        if (memberResultss.contains(Some(false))) {
          // Definitely not compatible
          Some(false)
        }
        else if (memberResultss.contains(None)) {
          // May satisfy
          None
        }
        else {
          // Definitely satisfies
          Some(true)
        }

      case (superDerived : DerivedSchemeType, _) =>
        // Didn't have an exact match - check our parent types
        stackedSatisfiesType(superDerived.parentType :: superStack.tail, testingStack) match {
          case Some(false) =>
            // Definitely doesn't match
            Some(false)

          case _ =>
            // May match a super type
            None
        }
      
      case (_, testingDerived : DerivedSchemeType) =>
        stackedSatisfiesType(superStack, testingDerived.parentType :: testingStack.tail)
    }
  }

  /** Determines if a Scheme type satisfies another
    *
    * Some(true) indicates all values of test testing type satisfy the super type. Some(false) indicates no values of
    * the testing type satisfy the super type.  None indicates that some values of the testing type satisfy the super
    * type.
    */
  def apply(superType : SchemeType, testingType : SchemeType) : Option[Boolean] = {
    stackedSatisfiesType(superType :: Nil, testingType :: Nil)
  }
}
