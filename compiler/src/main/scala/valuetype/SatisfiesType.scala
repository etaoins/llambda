package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}

object SatisfiesType {
  private def argTypeToListType(fixedArgs : List[SchemeType], restArgMemberTypeOpt : Option[SchemeType]) : SchemeType = {
    val fixedArgCdr = restArgMemberTypeOpt match {
      case None =>
        EmptyListType

      case Some(memberType) =>
        UniformProperListType(DirectSchemeTypeRef(memberType))
    }

    fixedArgs.foldRight(fixedArgCdr) { case (fixedArgType, cdrType) =>
      SpecificPairType(
        DirectSchemeTypeRef(fixedArgType),
        DirectSchemeTypeRef(cdrType)
      )
    }
  }

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

  private def mergeNonUnionMemberTypeResults(memberResults : Set[Option[Boolean]]) : Option[Boolean] =
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

  private def satifiesProcedureType(superType : ProcedureType, testingType : ProcedureType) : Option[Boolean] =
    (superType, testingType) match {
      case (ProcedureType(superFixedArgTypes, superRestArgMemberTypeOpt, superReturnType),
            ProcedureType(testingFixedArgTypes, testingRestArgMemberTypeOpt, testingReturnType)) =>
        // Construct a list type based on our arguments
        val superArgList = argTypeToListType(superFixedArgTypes, superRestArgMemberTypeOpt)
        val testingArgList = argTypeToListType(testingFixedArgTypes, testingRestArgMemberTypeOpt)
        // Test the list type - note that super/test is reversed because argument types are contravariant
        val argListResult = apply(testingArgList, superArgList)

        val returnTypeResult = ConvertibleToReturnType(superReturnType, testingReturnType)

        val allResults = Set(argListResult, returnTypeResult)

        if (allResults.contains(Some(false))) {
          Some(false)
        }
        else if (allResults.contains(None)) {
          None
        }
        else {
          Some(true)
        }
    }

  /** Compares two applicable types
    *
    * This ensures that each signature in the testing type is satisfied by at least one signature in the super type
    */
  private def satifiesApplicableType(superType : ApplicableType, testingType : ApplicableType) : Option[Boolean] = {
    val allSignatureResults = (testingType.signatures.map { testingSignature =>
      val testingSignatureResults = superType.signatures map { superSignature =>
        satifiesProcedureType(superSignature, testingSignature)
      }

      if (testingSignatureResults.contains(Some(true))) {
        // At least one of these signatures is definitely satisfies
        Some(true)
      }
      else if (testingSignatureResults.contains(None)) {
        // One signature can possibly match
        None
      }
      else {
        Some(false)
      }
    }).toSet

    if (allSignatureResults == Set(Some(true))) {
      // All signatures definitely satisfy
      Some(true)
    }
    else if (allSignatureResults == Set(Some(false))) {
      // No signatures match
      Some(false)
    }
    else {
      // Some applications match
      None
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

      case (superValue : LiteralValueType, testingValue : LiteralValueType) =>
        Some(superValue == testingValue)

      case (superRecord : RecordType, testingRecord : RecordType) =>
        // Record types satisfy themselves
        Some(superRecord eq testingRecord)
      
      case (superPair : PairType, testingPair : PairType) =>
        // Pairs satisfy their more general pairs
        val memberResults = Set(
          satisfiesTypeRef(superPair.carTypeRef, superStack, testingPair.carTypeRef, testingStack),
          satisfiesTypeRef(superPair.cdrTypeRef, superStack, testingPair.cdrTypeRef, testingStack)
        )

        mergeNonUnionMemberTypeResults(memberResults)

      case (UniformVectorType(superMemberTypeRef), UniformVectorType(testingMemberTypeRef)) =>
        satisfiesTypeRef(superMemberTypeRef, superStack, testingMemberTypeRef, testingStack)

      case (UniformVectorType(DirectSchemeTypeRef(AnySchemeType)), SchemeTypeAtom(ct.VectorCell)) =>
        Some(true)
      
      case (SpecificVectorType(superMemberTypeRefs), SpecificVectorType(testingMemberTypeRefs)) =>
        if (superMemberTypeRefs.size != testingMemberTypeRefs.size) {
          // Different lengths
          Some(false)
        }
        else {
          val memberResults = (superMemberTypeRefs zip testingMemberTypeRefs) map {
            case (superMemberTypeRef, testingMemberTypeRef) =>
              satisfiesTypeRef(superMemberTypeRef, superStack, testingMemberTypeRef, testingStack)
          }
        
          mergeNonUnionMemberTypeResults(memberResults.toSet)
        }

      case (UniformVectorType(superMemberTypeRef), SpecificVectorType(testingMemberTypeRefs)) =>
        val memberResults = testingMemberTypeRefs.map { testingMemberTypeRef =>
          satisfiesTypeRef(superMemberTypeRef, superStack, testingMemberTypeRef, testingStack)
        }
          
        mergeNonUnionMemberTypeResults(memberResults.toSet)

      case (SpecificVectorType(superMemberTypeRefs), UniformVectorType(testingMemberTypeRef)) =>
        val memberResults = superMemberTypeRefs.map { superMemberTypeRef =>
          satisfiesTypeRef(superMemberTypeRef, superStack, testingMemberTypeRef, testingStack)
        }

        if (memberResults.contains(Some(false))) {
          // Definitely doesn't match
          Some(false)
        }
        else {
          // May match depending on the length of the uniform vector
          None
        }

      case (superApplicable : ApplicableType, testingApplicable : ApplicableType) =>
        satifiesApplicableType(superApplicable, testingApplicable)

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
