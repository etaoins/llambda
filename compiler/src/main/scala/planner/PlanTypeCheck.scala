package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}

object PlanTypeCheck {
  private def testDerivedNonUnionType(
      plan : PlanWriter,
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      testingType : vt.NonUnionSchemeType,
      isTypeBuilder : (PlanWriter) => ps.TempValue,
      isNotTypeBuilder : (PlanWriter) => ps.TempValue
  ) : ps.TempValue = testingType match {
    case recordType : vt.RecordType =>
      // Cast the value to its boxed form
      val recordCellTemp = ps.RecordTemp()
      plan.steps += ps.CastCellToTypeUnchecked(recordCellTemp, valueTemp, recordType.cellType)

      val classMatchedPred = ps.Temp(vt.Predicate)
      plan.steps += ps.TestRecordLikeClass(classMatchedPred, recordCellTemp, recordType) 

      plan.buildCondBranch(classMatchedPred, isTypeBuilder, isNotTypeBuilder)
    
    case vt.SchemeTypeAtom(cellType) =>
      val isCellTypePred = ps.Temp(vt.Predicate)
      plan.steps += ps.TestCellType(isCellTypePred, valueTemp, cellType, valueType.possibleCellRepresentations)
        
      plan.buildCondBranch(isCellTypePred, isTypeBuilder, isNotTypeBuilder) 
  }
  
  private def testNonUnionType(
      plan : PlanWriter,
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      testingType : vt.NonUnionSchemeType,
      isTypeBuilder : (PlanWriter) => ps.TempValue,
      isNotTypeBuilder : (PlanWriter) => ps.TempValue
  ) : ps.TempValue = valueType.satisfiesType(testingType) match {
    case Some(true) =>
      isTypeBuilder(plan)

    case Some(false) =>
      isNotTypeBuilder(plan)

    case None =>
      testingType match {
        case derivedType : vt.DerivedSchemeType =>
          // Make sure our parent type is satisfied
          testNonUnionType(plan, valueTemp, valueType, derivedType.parentType, 
            isTypeBuilder={ isTypePlan =>
              val remainingType = valueType & derivedType.parentType

              // Now we can test oureslves
              testDerivedNonUnionType(plan, valueTemp, remainingType, testingType, isTypeBuilder, isNotTypeBuilder)
            },
            isNotTypeBuilder=isNotTypeBuilder
          )

        case topLevelType =>
          testDerivedNonUnionType(plan, valueTemp, valueType, testingType, isTypeBuilder, isNotTypeBuilder)
      }
  }  

  private def testUnionTypeRecursively(
      plan : PlanWriter,
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      memberTypes : List[vt.NonUnionSchemeType],
      isTypeBuilder : (PlanWriter) => ps.TempValue,
      isNotTypeBuilder : (PlanWriter) => ps.TempValue
  ) : ps.TempValue = memberTypes match {
    case testingType :: restTypes =>
      val tailNotTypeBuilder = if (restTypes.isEmpty) {
        isNotTypeBuilder
      }
      else {
        { isNotTypePlan : PlanWriter =>
          val remainingType = valueType - testingType
          testUnionTypeRecursively(isNotTypePlan, valueTemp, remainingType, restTypes, isTypeBuilder, isNotTypeBuilder)
        }
      }

      testNonUnionType(plan, valueTemp, valueType, testingType,
        isTypeBuilder=isTypeBuilder,
        isNotTypeBuilder=tailNotTypeBuilder
      )

    case Nil =>
      // Empty union type - no need for cond branch
      isNotTypeBuilder(plan)
  }

  /** Checks that a temp value has the passed Scheme type
    *
    * isTypeBuilder will be invoked with the branch taken if the temp value satisfies the Scheme type. isNotTypeBuilder
    * will be invoked with the branch taken in the failure case. Their return type values are phi'ed together as the
    * return value from this function
    */
  def apply(
      valueTemp : ps.TempValue,
      valueType : vt.SchemeType,
      testingType : vt.SchemeType,
      isTypeBuilder : (PlanWriter) => ps.TempValue,
      isNotTypeBuilder : (PlanWriter) => ps.TempValue
  )(implicit plan : PlanWriter) : ps.TempValue = testingType match {
    case nonUnion : vt.NonUnionSchemeType =>
      testNonUnionType(plan, valueTemp, valueType, nonUnion, isTypeBuilder, isNotTypeBuilder)

    case vt.UnionType(memberTypes) =>
      testUnionTypeRecursively(plan, valueTemp, valueType, memberTypes.toList, isTypeBuilder, isNotTypeBuilder)
  }
}
