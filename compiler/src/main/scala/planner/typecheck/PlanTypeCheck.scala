package io.llambda.compiler.planner.typecheck
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{PlanWriter, BoxedValue}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}

object PlanTypeCheck {
  /** Used to track if we're planning a predicate */
  private case class PlanningPredicateProc(
    testType : vt.SchemeType,
    nativeSymbol : String
  )

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
      predProcOpt : Option[PlanningPredicateProc],
      checkValue : BoxedValue,
      valueType : vt.SchemeType,
      testCarType : vt.SchemeType,
      testCdrType : vt.SchemeType
  ) : CheckResult = {
    // Determine what we know about the car and cdr types already
    // This can speed up their type checks
    val (knownCarType, knownCdrType) = valueType match {
      case pairType : vt.PairType =>
        (pairType.carType, pairType.cdrType)

      case _ =>
        (vt.AnySchemeType, vt.AnySchemeType)
    }

    branchOnType(plan, predProcOpt, checkValue, valueType, vt.AnyPairType, isTypePlanner=Some({
      (isPairPlan, remainingType) =>
        val pairCellTemp = checkValue.castToCellTempValue(ct.PairCell)(isPairPlan)

        // Test the car first - the order doesn't actually matter here
        val carTemp = ps.CellTemp(ct.DatumCell)
        isPairPlan.steps += ps.LoadPairCar(carTemp, pairCellTemp)

        val checkableCar = BoxedValue(ct.DatumCell, carTemp)
        branchOnType(isPairPlan, predProcOpt, checkableCar, knownCarType, testCarType, isTypePlanner=Some({
          (carSatifiesPlan, _) =>
            // car matched, load the cdr
            val cdrTemp = ps.CellTemp(ct.DatumCell)
            carSatifiesPlan.steps += ps.LoadPairCdr(cdrTemp, pairCellTemp)

            val checkableCdr = BoxedValue(ct.DatumCell, cdrTemp)
            branchOnType(carSatifiesPlan,  predProcOpt, checkableCdr, knownCdrType, testCdrType)
        }))
    }))
  }
  
  private def testNonUnionType(
      plan : PlanWriter,
      predProcOpt : Option[PlanningPredicateProc],
      checkValue : BoxedValue,
      valueType : vt.SchemeType,
      testType : vt.NonUnionSchemeType
  ) : CheckResult = {
    testType match {
      case recordType : vt.RecordType =>
        branchOnType(plan, predProcOpt, checkValue, valueType, recordType.parentType, isTypePlanner=Some({
          (isRecordPlan, remainingType) =>
            testRecordClass(isRecordPlan, checkValue, remainingType, recordType)
        }))

      case vt.SpecificPairType(testCarType, testCdrType) =>
        testPairType(plan, predProcOpt, checkValue, valueType, testCarType, testCdrType)
      
      case properListType @ vt.ProperListType(memberType) =>
        // This must either be the empty list or a pair with a car of member type and a cdr of the same proper list
        // type
        branchOnType(plan, predProcOpt, checkValue, valueType, vt.EmptyListType, isNotTypePlanner=Some({
          (isNotEmptyListPlan, remainingType) =>
            testPairType(isNotEmptyListPlan, predProcOpt, checkValue, remainingType, memberType, properListType)
        }))

      case vt.ConstantBooleanType(value) =>
        val castTemp = checkValue.castToCellTempValue(ct.BooleanCell)(plan)

        // This works because booleans are preconstructed
        val expectedTemp = ps.Temp(vt.BooleanType)
        plan.steps += ps.CreateBooleanCell(expectedTemp, value)

        val valueMatchedPred = ps.Temp(vt.Predicate)
        plan.steps += ps.IntegerCompare(valueMatchedPred, ps.CompareCond.Equal, None, castTemp, expectedTemp)

        DynamicResult(valueMatchedPred)
      
      case vt.SchemeTypeAtom(cellType) =>
        val possibleCellTypes = flattenType(valueType).flatMap(_.cellType.concreteTypes) 

        val isCellTypePred = ps.Temp(vt.Predicate)
        plan.steps += ps.TestCellType(isCellTypePred, checkValue.tempValue, cellType, possibleCellTypes)
        DynamicResult(isCellTypePred)
    }
  }

  private def testUnionTypeRecursively(
      plan : PlanWriter,
      predProcOpt : Option[PlanningPredicateProc],
      checkValue : BoxedValue,
      valueType : vt.SchemeType,
      memberTypes : List[vt.NonUnionSchemeType]
  ) : CheckResult = memberTypes match {
    case testType :: restTypes =>
      branchOnType(plan, predProcOpt, checkValue, valueType, testType, isNotTypePlanner=Some({
        (isNotTypePlan, remainingType) =>
          testUnionTypeRecursively(isNotTypePlan, predProcOpt, checkValue, remainingType, restTypes)
      }))

    case Nil =>
      // No types left
      StaticFalseResult
  }
  
  private def branchOnType(
      plan : PlanWriter,
      predProcOpt : Option[PlanningPredicateProc],
      checkValue : BoxedValue,
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
        // Have we either:
        // 1) Explicitly been asked to inline (e.g. while planning the type predicate itself(
        // 2) Been given a value with type information that would be lost calling a predicate. Exclude proper lists as
        //    they require a recursive function call anyway 
        // 3) Have an extremely trivial check to perform
        val shouldInline = mustInline || 
          (!(valueType eq vt.AnySchemeType) && !testType.isInstanceOf[vt.ProperListType]) || 
          testType.isInstanceOf[vt.SchemeTypeAtom]

        val testResult = if (shouldInline) {
          testType match {
            case nonUnion : vt.NonUnionSchemeType =>
              testNonUnionType(plan, predProcOpt, checkValue, valueType, nonUnion)

            case vt.UnionType(memberTypes) =>
              testUnionTypeRecursively(plan, predProcOpt, checkValue, valueType, memberTypes.toList)
          }
        }
        else {
          // Plan this out-of-line
          val (nativeSymbol, tailCall) = predProcOpt match {
            case Some(PlanningPredicateProc(predType, nativeSymbol)) if predType == testType =>
              // This is a recursive tail call to ourselves
              (nativeSymbol, true)

            case _ =>
              (TypePredicateProcForType(testType)(plan).nativeSymbol(plan), false)
          }

          val signature = TypePredicateProcSignature

          // Load the entry point for the predicate procedure
          val entryPointTemp = ps.EntryPointTemp()
          plan.steps += ps.CreateNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

          // Cast the value to datum*
          val datumValueTemp = checkValue.castToCellTempValue(ct.DatumCell)(plan)

          val resultPredTemp = ps.Temp(vt.Predicate)
          plan.steps += ps.Invoke(
            result=Some(resultPredTemp),
            signature=signature,
            entryPoint=entryPointTemp,
            arguments=List(ps.InvokeArgument(datumValueTemp)),
            tailCall=tailCall
          )

          DynamicResult(resultPredTemp) 
        }

        if (!isTypePlanner.isDefined && !isNotTypePlanner.isDefined) {
          // We can use this result directly - no need to generate a branch
          testResult
        }
        else {
          val testPred = testResult.toNativePred()(plan)

          val phiPred = plan.buildCondBranch(testPred, {isTypePlan =>
            // Now we can test oureslves
            val remainingType = valueType & testType
            
            val innerCheckResult = isTypePlanner.map { planner =>
              planner(isTypePlan, remainingType)
            } getOrElse StaticTrueResult

            innerCheckResult.toNativePred()(isTypePlan)
          },
          { isNotTypePlan =>
            // Not our parent type
            val remainingType = valueType - testType
            
            val innerCheckResult = isNotTypePlanner.map { planner =>
              planner(isNotTypePlan, remainingType)
            } getOrElse StaticFalseResult

            innerCheckResult.toNativePred()(isNotTypePlan)
          })

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
    val predProcOpt = selfSymbolOpt map { selfSymbol =>
      PlanningPredicateProc(testType, selfSymbol)
    }

    branchOnType(plan, predProcOpt, checkValue, valueType, testType, mustInline=predProcOpt.isDefined)
  }
}
