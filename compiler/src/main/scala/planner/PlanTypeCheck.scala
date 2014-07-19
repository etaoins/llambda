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
    val containsGenericRecordType = flattenedType.exists(recordCellType.satisfiesType(_) == Some(true))

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
  
  private def requireType(
      plan : PlanWriter,
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      requiredType : vt.NonUnionSchemeType,
      innerTypeCheck : (PlanWriter, vt.SchemeType) => ps.TempValue
  ) : ps.TempValue = {
    val isRequiredTypePred = testNonUnionType(plan, valueTemp, valueType, requiredType) 

    plan.buildCondBranch(isRequiredTypePred, {isRequiredTypePlan =>
      // Now we can test oureslves
      val remainingType = requiredType & valueType
      innerTypeCheck(isRequiredTypePlan, remainingType)
    },
    { isNotRequiredTypePlan =>
      // Not our parent type
      predicateTemp(isNotRequiredTypePlan, false)
    })
  }

  private def testNonUnionType(
      plan : PlanWriter,
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      testingType : vt.NonUnionSchemeType
  ) : ps.TempValue = valueType.satisfiesType(testingType) match {
    case Some(knownValue) =>
      predicateTemp(plan, knownValue)

    case None =>
      testingType match {
        case recordType : vt.RecordType =>
          requireType(plan, valueTemp, valueType, recordType.parentType, { (isRecordPlan, remainingType) =>
            testRecordClass(isRecordPlan, valueTemp, remainingType, recordType)
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
          val possibleCellTypes = flattenType(valueType).map(_.cellType) 

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
      val isTestingTypePred = testNonUnionType(plan, valueTemp, valueType, testingType)
      
      plan.buildCondBranch(isTestingTypePred, {isTypePlan =>
        predicateTemp(isTypePlan, true)
      },
      { isNotTypePlan  =>
        val remainingType = valueType - testingType
        testUnionTypeRecursively(isNotTypePlan, valueTemp, remainingType, restTypes)
      })


    case Nil =>
      predicateTemp(plan, false)
  }

  /** Checks that a temp value has the passed Scheme type and returns a predicate */
  def apply(
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      testingType : vt.SchemeType
  )(implicit plan : PlanWriter) : ps.TempValue = testingType match {
    case nonUnion : vt.NonUnionSchemeType =>
      testNonUnionType(plan, valueTemp, valueType, nonUnion)

    case vt.UnionType(memberTypes) =>
      testUnionTypeRecursively(plan, valueTemp, valueType, memberTypes.toList)
  }
}
