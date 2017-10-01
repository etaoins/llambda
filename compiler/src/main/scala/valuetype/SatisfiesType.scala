package io.llambda.compiler.valuetype


object SatisfiesType {
  private def satisfiesTypeRef(
      superTypeRef: SchemeTypeRef,
      superStack: SchemeType.Stack,
      testingTypeRef: SchemeTypeRef,
      testingStack: SchemeType.Stack
  ): Option[Boolean] = {
    val superType = superTypeRef.resolve(superStack)
    val testingType = testingTypeRef.resolve(testingStack)

    if (superStack.contains(superType) && testingStack.contains(testingType)) {
      Some(true)
    }
    else {
      stackedSatisfiesType(superType :: superStack, testingType :: testingStack)
    }
  }

  private def mergeNonUnionMemberTypeResults(memberResults: Set[Option[Boolean]]): Option[Boolean] =
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

  private def satisfiesProcedureType(superType: ProcedureType, testingType: ProcedureType): Option[Boolean] =
    (superType, testingType) match {
      case (ProcedureType(superMandatoryArgTypes, superOptionalArgTypes, superRestArgMemberTypeOpt, superReturnType),
            ProcedureType(testingMandatoryArgTypes, testingOptionalArgTypes, testingRestArgMemberTypeOpt, testingReturnType)) =>
        // Construct a list type based on our arguments
        val superArgList = FormalsToListType(superMandatoryArgTypes, superOptionalArgTypes, superRestArgMemberTypeOpt)
        val testingArgList = FormalsToListType(testingMandatoryArgTypes, testingOptionalArgTypes, testingRestArgMemberTypeOpt)
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

  def stackedSatisfiesType(superStack: SchemeType.Stack, testingStack: SchemeType.Stack): Option[Boolean] = {
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

      case (superAtom: SchemeTypeAtom, testingAtom: SchemeTypeAtom) =>
        // Atoms are easy
        Some(superAtom == testingAtom)

      case (superValue: LiteralValueType, testingValue: LiteralValueType) =>
        Some(superValue == testingValue)

      case (superRecord: RecordType, testingRecord: RecordType) =>
        if (testingRecord.isEqualToOrChildOf(superRecord)) {
          // Testing record is a child record
          Some(true)
        }
        else if (superRecord.isEqualToOrChildOf(testingRecord)) {
          // Testing record is a parent of child record
          None
        }
        else {
          // No direct parent-child relationship
          Some(false)

        }

      case (superExternal: ExternalRecordType, testingRecord: RecordType) =>
        Some(false)

      case (superRecord: RecordType, testingExternal: ExternalRecordType) =>
        Some(false)

      case (superExternal: ExternalRecordType, testingExternal: ExternalRecordType) =>
        Some(superExternal eq testingExternal)

      case (superPair: PairType, testingPair: PairType) =>
        // Pairs satisfy their more general pairs
        val memberResults = Set(
          satisfiesTypeRef(superPair.carTypeRef, superStack, testingPair.carTypeRef, testingStack),
          satisfiesTypeRef(superPair.cdrTypeRef, superStack, testingPair.cdrTypeRef, testingStack)
        )

        mergeNonUnionMemberTypeResults(memberResults)

      case (superHashMap: HashMapType, testingHashMap: HashMapType) =>
        val memberResults = Set(
          stackedSatisfiesType(superHashMap.keyType :: superStack, testingHashMap.keyType :: testingStack),
          stackedSatisfiesType(superHashMap.valueType :: superStack, testingHashMap.valueType :: testingStack)
        )

        mergeNonUnionMemberTypeResults(memberResults)

      case (superProcedure: ProcedureType, testingProcedure: ProcedureType) =>
        satisfiesProcedureType(superProcedure, testingProcedure)

      case (superDerived: DerivedSchemeType, _) =>
        // Didn't have an exact match - check our parent types
        stackedSatisfiesType(superDerived.parentType :: superStack.tail, testingStack) match {
          case Some(false) =>
            // Definitely doesn't match
            Some(false)

          case _ =>
            // May match a super type
            None
        }

      case (_, testingDerived: DerivedSchemeType) =>
        stackedSatisfiesType(superStack, testingDerived.parentType :: testingStack.tail)
    }
  }

  /** Determines if a Scheme type satisfies another
    *
    * Some(true) indicates all values of test testing type satisfy the super type. Some(false) indicates no values of
    * the testing type satisfy the super type.  None indicates that some values of the testing type satisfy the super
    * type.
    */
  def apply(superType: SchemeType, testingType: SchemeType): Option[Boolean] = {
    stackedSatisfiesType(superType :: Nil, testingType :: Nil)
  }
}
