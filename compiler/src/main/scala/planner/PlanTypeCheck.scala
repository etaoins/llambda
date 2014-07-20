package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}

object PlanTypeCheck {
  private def predicateTemp(plan : PlanWriter, value : Boolean) : ps.TempValue = {
    val predTemp = ps.Temp(vt.Predicate)
    plan.steps += ps.CreateNativeInteger(predTemp, if (value) 1 else 0, vt.Predicate.bits)
    predTemp
  }

  private def flattenType(schemeType : vt.SchemeType) : Set[vt.NonUnionSchemeType] = schemeType match {
    case nonUnion : vt.NonUnionSchemeType => Set(nonUnion)
    case vt.UnionType(memberTypes)        => memberTypes
  }

  private def testRecordClass(
    plan : PlanWriter,
    valueTemp : ps.TempValue,
    valueType : vt.SchemeType,
    recordType : vt.RecordType
  ) : ps.TempValue = {
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
    val recordCellTemp = ps.RecordTemp()
    plan.steps += ps.CastCellToTypeUnchecked(recordCellTemp, valueTemp, recordType.cellType)

    val classMatchedPred = ps.Temp(vt.Predicate)
    plan.steps += ps.TestRecordLikeClass(classMatchedPred, recordCellTemp, recordType, possibleTypesOpt) 
    classMatchedPred
  }
  
  private def testNonUnionType(
      plan : PlanWriter,
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      testingType : vt.NonUnionSchemeType
  ) : ps.TempValue = {
    testingType match {
      case recordType : vt.RecordType =>
        branchOnType(plan, valueTemp, valueType, recordType.parentType, isTypePlanner={
          (isRecordPlan, remainingType) =>
            testRecordClass(isRecordPlan, valueTemp, remainingType, recordType)
        })

      case vt.SpecificPairType(testingCarType, testingCdrType) =>
        // Determine what we know about the car and cdr types already
        // This can speed up their type checks
        val (knownCarType, knownCdrType) = valueType match {
          case pairType : vt.PairType =>
            (pairType.carType, pairType.cdrType)

          case _ =>
            (vt.AnySchemeType, vt.AnySchemeType)
        }

        branchOnType(plan, valueTemp, valueType, vt.AnyPairType, isTypePlanner={
          (isPairPlan, remainingType) =>
            val pairCellTemp = ps.CellTemp(ct.PairCell)
            plan.steps += ps.CastCellToTypeUnchecked(pairCellTemp, valueTemp, ct.PairCell)

            // Test the car first - the order doesn't actually matter here
            val carTemp = ps.CellTemp(ct.DatumCell)
            plan.steps += ps.LoadPairCar(carTemp, pairCellTemp)

            branchOnType(isPairPlan, carTemp, knownCarType, testingCarType, isTypePlanner={
              (carSatifiesPlan, _) =>
                // car matched, load the cdr
                val cdrTemp = ps.CellTemp(ct.DatumCell)
                plan.steps += ps.LoadPairCdr(cdrTemp, pairCellTemp)

                branchOnType(carSatifiesPlan, cdrTemp, knownCdrType, testingCdrType)
            })
        })

      case vt.ConstantBooleanType(value) =>
        val castTemp = ps.Temp(vt.BooleanType) 
        plan.steps += ps.CastCellToTypeUnchecked(castTemp, valueTemp, ct.BooleanCell)

        // This works because booleans are preconstructed
        val expectedTemp = ps.Temp(vt.BooleanType)
        plan.steps += ps.CreateBooleanCell(expectedTemp, value)

        val valueMatchedPred = ps.Temp(vt.Predicate)
        plan.steps += ps.IntegerCompare(valueMatchedPred, ps.CompareCond.Equal, None, castTemp, expectedTemp)

        valueMatchedPred
      
      case vt.SchemeTypeAtom(cellType) =>
        val possibleCellTypes = flattenType(valueType).flatMap(_.cellType.concreteTypes) 

        val isCellTypePred = ps.Temp(vt.Predicate)
        plan.steps += ps.TestCellType(isCellTypePred, valueTemp, cellType, possibleCellTypes)
        isCellTypePred
    }
  }

  private def testUnionTypeRecursively(
      plan : PlanWriter,
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      memberTypes : List[vt.NonUnionSchemeType]
  ) : ps.TempValue = memberTypes match {
    case testingType :: restTypes =>
      branchOnType(plan, valueTemp, valueType, testingType, isNotTypePlanner={
        (isNotTypePlan, remainingType) =>
          testUnionTypeRecursively(isNotTypePlan, valueTemp, remainingType, restTypes)
      })

    case Nil =>
      predicateTemp(plan, false)
  }
  
  private def branchOnType(
      plan : PlanWriter,
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      testingType : vt.SchemeType,
      isTypePlanner : ((PlanWriter, vt.SchemeType) => ps.TempValue) = { (plan, _) => predicateTemp(plan, true) },
      isNotTypePlanner : ((PlanWriter, vt.SchemeType) => ps.TempValue) = { (plan, _) => predicateTemp(plan, false) }
  ) : ps.TempValue =  {
    vt.SatisfiesType(testingType, valueType) match {
      case Some(true) =>
        isTypePlanner(plan, valueType)

      case Some(false) =>
        isNotTypePlanner(plan, valueType)

      case None =>
        val isTypePred = testingType match {
          case nonUnion : vt.NonUnionSchemeType =>
            testNonUnionType(plan, valueTemp, valueType, nonUnion)

          case vt.UnionType(memberTypes) =>
            testUnionTypeRecursively(plan, valueTemp, valueType, memberTypes.toList)
        }

        plan.buildCondBranch(isTypePred, {isTypePlan =>
          // Now we can test oureslves
          val remainingType = valueType & testingType
          isTypePlanner(isTypePlan, remainingType)
        },
        { isNotTypePlan =>
          // Not our parent type
          val remainingType = valueType - testingType
          isNotTypePlanner(isNotTypePlan, remainingType)
        })
    }
  }

  /** Plans a type check for a given value and testing type
    *
    * @param  valueTemp    Temp value to plan a type test for. This value may be of any cell type.
    * @param  valueType    Known Scheme value for the type. More specific Scheme types may produce more a efficient
    *                      type check plan
    * @param  testingType  Scheme type to test if valueTemp is a member of
    * @return Predicate true if valueTemp satisfies testingType, false otherwise
    */
  def apply(valueTemp : ps.TempValue, valueType : vt.SchemeType, testingType : vt.SchemeType)(implicit plan : PlanWriter) : ps.TempValue = {
    branchOnType(plan, valueTemp, valueType, testingType)
  }
}
