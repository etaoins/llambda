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

  private def testDerivedNonUnionType(
      plan : PlanWriter,
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      testingType : vt.NonUnionSchemeType
  ) : ps.TempValue = testingType match {
    case recordType : vt.RecordType =>
      // Cast the value to its boxed form
      val recordCellTemp = ps.RecordTemp()
      plan.steps += ps.CastCellToTypeUnchecked(recordCellTemp, valueTemp, recordType.cellType)

      val classMatchedPred = ps.Temp(vt.Predicate)
      plan.steps += ps.TestRecordLikeClass(classMatchedPred, recordCellTemp, recordType) 
      classMatchedPred
    
    case vt.SchemeTypeAtom(cellType) =>
      val isCellTypePred = ps.Temp(vt.Predicate)
      plan.steps += ps.TestCellType(isCellTypePred, valueTemp, cellType, valueType.possibleCellRepresentations)
      isCellTypePred
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
        case derivedType : vt.DerivedSchemeType =>
          // Make sure our parent type is satisfied
          val isParentTypePred = testNonUnionType(plan, valueTemp, valueType, derivedType.parentType) 

          plan.buildCondBranch(isParentTypePred, {isTypePlan =>
            val remainingType = valueType & derivedType.parentType

            // Now we can test oureslves
            testDerivedNonUnionType(isTypePlan, valueTemp, remainingType, testingType)
          },
          { isNotTypePlan =>
            // Not our parent type
            predicateTemp(isNotTypePlan, false)
          })

        case topLevelType =>
          testDerivedNonUnionType(plan, valueTemp, valueType, testingType)
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
