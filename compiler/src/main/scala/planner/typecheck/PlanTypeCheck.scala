package io.llambda.compiler.planner.typecheck
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{PlanWriter, BoxedValue}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{InternalCompilerErrorException, ImpossibleTypeConversionException}

object PlanTypeCheck {
  private def unrolledTypeRef(schemeTypeRef : vt.SchemeTypeRef) : vt.SchemeType = {
    // We pre-unroll our types so we should never encounter a recursive ref
    schemeTypeRef match {
      case vt.DirectSchemeTypeRef(directType) =>
        directType

      case _ =>
        throw new InternalCompilerErrorException("Encountered recursive reference - type should be unrolled")
    }
  }

  private def flattenType(schemeType : vt.SchemeType) : Set[vt.NonUnionSchemeType] = schemeType match {
    case nonUnion : vt.NonUnionSchemeType => Set(nonUnion)
    case vt.UnionType(memberTypes)        => memberTypes
  }

  private def testRecordClass(
      plan : PlanWriter,
      checkValue : BoxedValue,
      valueType : vt.SchemeType,
      recordType : vt.RecordType
  ) : CheckResult = {
    val flattenedType = flattenType(valueType)

    // If we contain a generic record type we can be of any record class
    val recordCellType = vt.SchemeTypeAtom(ct.RecordCell)
    val containsGenericRecordType = flattenedType.exists(vt.SatisfiesType(_, recordCellType) == Some(true))

    val possibleTypesOpt = if (containsGenericRecordType) {
      None
    }
    else {
      Some(flattenType(valueType) collect {
        case recordType : vt.RecordType =>
          recordType
      } : Set[vt.RecordLikeType])
    }

    // Cast the value to its boxed form
    val recordCellTemp = checkValue.castToCellTempValue(recordType.cellType)(plan)

    val classMatchedPred = ps.Temp(vt.Predicate)
    plan.steps += ps.TestRecordLikeClass(classMatchedPred, recordCellTemp, recordType, possibleTypesOpt) 
    DynamicResult(classMatchedPred)
  }

  private def testPairType(
      plan : PlanWriter,
      checkValue : BoxedValue,
      valueType : vt.SchemeType,
      testCarType : vt.SchemeType,
      testCdrType : vt.SchemeType
  ) : CheckResult = {
    // Determine what we know about the car and cdr types already
    // This can speed up their type checks
    val (knownCarType, knownCdrType) = valueType match {
      case pairType : vt.PairType =>
        (unrolledTypeRef(pairType.carTypeRef), unrolledTypeRef(pairType.cdrTypeRef))

      case _ =>
        (vt.AnySchemeType, vt.AnySchemeType)
    }

    branchOnType(plan, checkValue, valueType, vt.AnyPairType, isTypePlanner=Some({
      (isPairPlan, remainingType) =>
        val pairCellTemp = checkValue.castToCellTempValue(ct.PairCell)(isPairPlan)

        // Test the car first - the order doesn't actually matter here
        val carTemp = ps.CellTemp(ct.AnyCell)
        isPairPlan.steps += ps.LoadPairCar(carTemp, pairCellTemp)

        val checkableCar = BoxedValue(ct.AnyCell, carTemp)
        branchOnType(isPairPlan, checkableCar, knownCarType, testCarType, isTypePlanner=Some({
          (carSatifiesPlan, _) =>
            // car matched, load the cdr
            val cdrTemp = ps.CellTemp(ct.AnyCell)
            carSatifiesPlan.steps += ps.LoadPairCdr(cdrTemp, pairCellTemp)

            val checkableCdr = BoxedValue(ct.AnyCell, cdrTemp)
            branchOnType(carSatifiesPlan, checkableCdr, knownCdrType, testCdrType)
        }))
    }))
  }

  private def testUniformVectorType(
      plan : PlanWriter,
      checkValue : BoxedValue,
      valueType : vt.SchemeType,
      testMemberType : vt.SchemeType
  ) : CheckResult = {
    branchOnType(plan, checkValue, valueType, vt.SchemeTypeAtom(ct.VectorCell), isTypePlanner=Some({
      (isVectorPlan, remainingType) =>
        // Find our value's member type
        val knownMemberType = valueType match {
          case vt.UniformVectorType(memberTypeRef) =>
            unrolledTypeRef(memberTypeRef)

          case _ =>
            vt.AnySchemeType
        }
        
        if (vt.SatisfiesType(testMemberType, knownMemberType) == Some(true)) {
          // No need to loop
          StaticTrueResult
        }
        else {
          val vectorTemp = checkValue.castToCellTempValue(ct.VectorCell)(isVectorPlan)

          // Find the length of the vector
          val vectorLengthTemp = ps.Temp(vt.UInt32)
          isVectorPlan.steps += ps.LoadVectorLength(vectorLengthTemp, vectorTemp)

          // Load the vector elements pointer
          val elementsTemp = ps.VectorElementsTemp()
          isVectorPlan.steps += ps.LoadVectorElementsData(elementsTemp, vectorTemp)

          // Create a temp for the vector index
          val vectorIndexTemp = ps.Temp(vt.UInt32)

          // Fork the plan
          val loopPlan = isVectorPlan.forkPlan()

          val loopResultPred ={
            // Load the element
            val elementTemp = ps.Temp(vt.AnySchemeType) 
            loopPlan.steps += ps.LoadVectorElement(elementTemp, vectorTemp, elementsTemp, vectorIndexTemp) 

            // Check its type
            val checkableElement = BoxedValue(ct.AnyCell, elementTemp)
            val memberResult = branchOnType(loopPlan, checkableElement, knownMemberType, testMemberType)

            // Create a predicate for the result
            memberResult.toNativePred()(loopPlan)
          }

          val forAllResultPred = ps.Temp(vt.Predicate)
          isVectorPlan.steps += ps.ForAll(
            result=forAllResultPred,
            loopCountValue=vectorLengthTemp,
            loopIndexValue=vectorIndexTemp,
            loopSteps=loopPlan.steps.toList,
            loopResultPred=loopResultPred
          )

          DynamicResult(forAllResultPred)
        }
    }))
  }

  private def testVectorElementTypes(
      plan : PlanWriter,
      vectorTemp : ps.TempValue,
      elementsTemp : ps.TempValue,
      typesToTest : Vector[(vt.SchemeType, vt.SchemeType)],
      index : Int
  ) : CheckResult = if (index >= typesToTest.size) {
    StaticTrueResult
  }
  else {
    val (valueMemberType, testMemberType) = typesToTest(index)

    val indexTemp = ps.Temp(vt.UInt32)
    plan.steps += ps.CreateNativeInteger(indexTemp, index, 32)

    val elementTemp = ps.Temp(vt.AnySchemeType)
    plan.steps += ps.LoadVectorElement(elementTemp, vectorTemp, elementsTemp, indexTemp)

    val checkableElement = BoxedValue(ct.AnyCell, elementTemp)
    branchOnType(plan, checkableElement, valueMemberType, testMemberType, Some({
      (isTypePlan, remainingType) =>
        testVectorElementTypes(plan, vectorTemp, elementsTemp, typesToTest, index + 1)
    }))
  }
      
  private def testSpecificVectorType(
      plan : PlanWriter,
      checkValue : BoxedValue,
      valueType : vt.SchemeType,
      testMemberTypes : Vector[vt.SchemeType]
  ) : CheckResult = {
    branchOnType(plan, checkValue, valueType, vt.SchemeTypeAtom(ct.VectorCell), isTypePlanner=Some({
      (isVectorPlan, remainingType) =>
        val expectedLength = testMemberTypes.size

        val vectorTemp = checkValue.castToCellTempValue(ct.VectorCell)(isVectorPlan)
          
        // Load the vector elements pointer
        val elementsTemp = ps.VectorElementsTemp()
        isVectorPlan.steps += ps.LoadVectorElementsData(elementsTemp, vectorTemp)

        // Find the length of the vector
        val vectorLengthTemp = ps.Temp(vt.UInt32)
        isVectorPlan.steps += ps.LoadVectorLength(vectorLengthTemp, vectorTemp)

        val expectedLengthTemp = ps.Temp(vt.UInt32)
        isVectorPlan.steps += ps.CreateNativeInteger(expectedLengthTemp, expectedLength, ct.VectorCell.lengthIrType.bits) 

        val lengthMatchTemp = ps.Temp(vt.Predicate)
        isVectorPlan.steps += ps.IntegerCompare(lengthMatchTemp, ps.CompareCond.Equal, None, vectorLengthTemp, expectedLengthTemp) 

        val resultTemp = isVectorPlan.buildCondBranch(lengthMatchTemp, { (lengthMatchPlan) =>
          val knownMemberTypes = valueType match {
            case vt.UniformVectorType(memberTypeRef) =>
              Vector.fill(expectedLength)(unrolledTypeRef(memberTypeRef))

            case vt.SpecificVectorType(memberTypeRefs) =>
              memberTypeRefs.map(unrolledTypeRef)

            case _ =>
              Vector.fill(expectedLength)(vt.AnySchemeType)
          }

          val typesToTest = knownMemberTypes.zip(testMemberTypes)
          val checkResult = testVectorElementTypes(lengthMatchPlan, vectorTemp, elementsTemp, typesToTest, 0)

          checkResult.toNativePred()(lengthMatchPlan)
        }, {lengthMismatchPlan =>
          val falseTemp = ps.Temp(vt.Predicate)
          lengthMismatchPlan.steps += ps.CreateNativeInteger(falseTemp, 0, vt.Predicate.bits)

          falseTemp
        })

        DynamicResult(resultTemp)
    }))
  }
  
  private def testNonUnionType(
      plan : PlanWriter,
      checkValue : BoxedValue,
      valueType : vt.SchemeType,
      testType : vt.NonUnionSchemeType
  ) : CheckResult = {
    testType match {
      case recordType : vt.RecordType =>
        branchOnType(plan, checkValue, valueType, recordType.parentType, isTypePlanner=Some({
          (isRecordPlan, remainingType) =>
            testRecordClass(isRecordPlan, checkValue, remainingType, recordType)
        }))

      case vt.SpecificPairType(testCarTypeRef, testCdrTypeRef) =>
        val testCarType = unrolledTypeRef(testCarTypeRef)
        val testCdrType = unrolledTypeRef(testCdrTypeRef)

        testPairType(plan, checkValue, valueType, testCarType, testCdrType)
      
      case vt.ConstantBooleanType(value) =>
        val castTemp = checkValue.castToCellTempValue(ct.BooleanCell)(plan)

        // This works because booleans are preconstructed
        val expectedTemp = ps.Temp(vt.BooleanType)
        plan.steps += ps.CreateBooleanCell(expectedTemp, value)

        val valueMatchedPred = ps.Temp(vt.Predicate)
        plan.steps += ps.IntegerCompare(valueMatchedPred, ps.CompareCond.Equal, None, castTemp, expectedTemp)

        DynamicResult(valueMatchedPred)
      
      case vt.SpecificVectorType(testMemberTypeRefs) =>
        val testMemberTypes = testMemberTypeRefs map { testMemberTypeRef =>
          unrolledTypeRef(testMemberTypeRef)
        }

        testSpecificVectorType(plan, checkValue, valueType, testMemberTypes)

      case vt.UniformVectorType(testMemberTypeRef) =>
        val testMemberType = unrolledTypeRef(testMemberTypeRef)
        testUniformVectorType(plan, checkValue, valueType, testMemberType)
      
      case vt.SchemeTypeAtom(cellType) =>
        val possibleCellTypes = flattenType(valueType).flatMap(_.cellType.concreteTypes) 

        val isCellTypePred = ps.Temp(vt.Predicate)
        plan.steps += ps.TestCellType(isCellTypePred, checkValue.tempValue, cellType, possibleCellTypes)
        DynamicResult(isCellTypePred)

      case _ : vt.ApplicableType =>
        throw new ImpossibleTypeConversionException(
          located=plan.activeContextLocated,
          message=s"Value of type ${valueType} does not statically satisfy procedure type ${testType}"
        )
    }
  }

  private def testUnionTypeRecursively(
      plan : PlanWriter,
      checkValue : BoxedValue,
      valueType : vt.SchemeType,
      memberTypes : List[vt.NonUnionSchemeType]
  ) : CheckResult = memberTypes match {
    case testType :: restTypes =>
      branchOnType(plan, checkValue, valueType, testType, isNotTypePlanner=Some({
        (isNotTypePlan, remainingType) =>
          testUnionTypeRecursively(isNotTypePlan, checkValue, remainingType, restTypes)
      }))

    case Nil =>
      // No types left
      StaticFalseResult
  }
  
  private def branchOnType(
      plan : PlanWriter,
      checkValue : => BoxedValue,
      valueType : vt.SchemeType,
      testType : vt.SchemeType,
      isTypePlanner : Option[((PlanWriter, vt.SchemeType) => CheckResult)] = None,
      isNotTypePlanner : Option[((PlanWriter, vt.SchemeType) => CheckResult)] = None,
      mustInline : Boolean = false
  ) : CheckResult =  {
    vt.SatisfiesType(testType, valueType) match {
      case Some(true) =>
        isTypePlanner.map { planner =>
          planner(plan, valueType)
        } getOrElse StaticTrueResult

      case Some(false) =>
        isNotTypePlanner.map { planner =>
          planner(plan, valueType)
        } getOrElse StaticFalseResult

      case None =>
        // Determine which predicate procs we're planning at the moment
        val planningTypes = plan.plannedTypePredicates.filter { case (schemeType, nativeSymbol) =>
          !plan.plannedFunctions.contains(nativeSymbol)
        }

        // Have we either:
        // 1) Explicitly been asked to inline (e.g. while planning the type predicate itself(
        // 2) Been given a value with type information that would be lost calling a predicate. Exclude types with 
        //    recursive references as they require a function call
        // 3) Have an extremely trivial check to perform
        val shouldInline = mustInline || 
          (!(valueType eq vt.AnySchemeType) && !vt.HasRecursiveRef(valueType) && !planningTypes.contains(valueType)) || 
          testType.isInstanceOf[vt.SchemeTypeAtom]

        val testResult = if (shouldInline) {
          // Unroll this type in case it's recursive
          testType.unrolled match {
            case nonUnion : vt.NonUnionSchemeType =>
              testNonUnionType(plan, checkValue, valueType, nonUnion)

            case vt.UnionType(memberTypes) =>
              // Determine a semi-stable order to test the member types in
              val orderedMemberTypes = memberTypes.toList.sortBy {
                case vt.SchemeTypeAtom(cellType) =>
                  // Prefer testing type atoms first because they're quick to test
                  cellType.typeId.toInt

                case _ =>
                  // This is the maximum cell type ID + 1
                  256
              }

              testUnionTypeRecursively(plan, checkValue, valueType, orderedMemberTypes)
          }
        }
        else {
          // Plan this out-of-line
          val nativeSymbol = SymbolForTypePredicateProc(plan, testType)
          val signature = TypePredicateProcSignature

          // Load the entry point for the predicate procedure
          val entryPointTemp = ps.EntryPointTemp()
          plan.steps += ps.CreateNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

          // Cast the value to datum*
          val datumValueTemp = checkValue.castToCellTempValue(ct.AnyCell)(plan)

          val resultPredTemp = ps.Temp(vt.Predicate)
          plan.steps += ps.Invoke(
            result=Some(resultPredTemp),
            signature=signature,
            entryPoint=entryPointTemp,
            arguments=List(ps.InvokeArgument(datumValueTemp))
          )

          DynamicResult(resultPredTemp) 
        }
          
        val isTypePlan = plan.forkPlan()
        val isNotTypePlan = plan.forkPlan()

        val isTypeResult = isTypePlanner.map({planner =>
          val remainingType = valueType & testType
          planner(isTypePlan, remainingType)
        }) getOrElse StaticTrueResult

        val isNotTypeResult = isNotTypePlanner.map({planner =>
          val remainingType = valueType - testType
          planner(isNotTypePlan, remainingType)
        }) getOrElse StaticFalseResult

        if ((isTypeResult == StaticTrueResult) && (isNotTypeResult == StaticFalseResult)) {
          // We can use our result directly without using the type branches
          testResult
        }
        else {
          val phiPred = ps.Temp(vt.Predicate)
          val testPred = testResult.toNativePred()(plan)
          val trueValue = isTypeResult.toNativePred()(isTypePlan)
          val falseValue = isNotTypeResult.toNativePred()(isNotTypePlan)

          // We need to phi the two type branches
          plan.steps += ps.CondBranch(
            result=phiPred,
            test=testPred,
            trueSteps=isTypePlan.steps.toList,
            trueValue=trueValue,
            falseSteps=isNotTypePlan.steps.toList,
            falseValue=falseValue
          )
          
          DynamicResult(phiPred)
        }
    }
  }

  /** Plans a type check for a given value and testing type
    *
    * This will either plan an inline type check or plan a call to a type predicate based on internal heuristics
    *
    * @param  checkValue     Value to plan a type test for. This value may be of any cell type.
    * @param  valueType      Known Scheme type of the value being checked 
    * @param  testType       Scheme type to test checkValue's membership in
    * @param  selfSymbolOpt  If defined the native symbol referring to the type predicate procedure being planned. This
    *                        is required for planning recursive types to recursive tail self calls
    * @return CheckResult indicating if checkValue satisfies testType
    */
  def apply(
      checkValue : => BoxedValue,
      valueType : vt.SchemeType,
      testType : vt.SchemeType,
      selfSymbolOpt : Option[String] = None
  )(implicit plan : PlanWriter) : CheckResult = {
    val predProcs = (selfSymbolOpt map { selfSymbol =>
      (testType -> selfSymbol)
    }).toMap

    branchOnType(plan, checkValue, valueType, testType, mustInline=selfSymbolOpt.isDefined)
  }
}
