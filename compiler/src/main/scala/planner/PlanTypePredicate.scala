package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

object PlanTypePredicate {
  private def testCellTypeRecursively(plan : PlanWriter, argumentTemp : ps.TempValue, possibleTypes : List[ct.ConcreteCellType]) : ps.TempValue = possibleTypes match {
    case testingType :: restTypes =>
      val isTestingTypePred = ps.Temp(vt.Predicate)
      plan.steps += ps.TestCellType(isTestingTypePred, argumentTemp, testingType)
        
      plan.buildCondBranch(isTestingTypePred, 
        {isTypePlan =>
          // We're the right type!
          val truePred = ps.Temp(vt.Predicate)
          isTypePlan.steps += ps.CreateNativeInteger(truePred, 1, vt.Predicate.bits)
          truePred
        },
        {isNotTypePlan =>
          // Test the rest of the types
          testCellTypeRecursively(isNotTypePlan, argumentTemp, restTypes)
        })

    case Nil =>
      // Ran out of types
      val falsePred = ps.Temp(vt.Predicate)
      plan.steps += ps.CreateNativeInteger(falsePred, 0, vt.Predicate.bits)
      falsePred
  }

  def apply(schemeType : vt.SchemeType)(implicit parentPlan : PlanWriter) : PlannedFunction =  {
    // Determine our signature
    val predicateSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      hasRestArg=false,
      // We must be able to take any data type without erroring out
      fixedArgs=List(vt.IntrinsicCellType(ct.DatumCell)),
      returnType=Some(vt.CBool),
      attributes=Set()
    )
    
    // We only have a single argument
    val argumentTemp = ps.CellTemp(ct.DatumCell)
    
    val plan = parentPlan.forkPlan()

    // Find our concrete cell types
    val concreteCellTypes = schemeType.cellType.concreteTypes.toList

    val isExpectedCellPred = testCellTypeRecursively(plan, argumentTemp, concreteCellTypes)
      
    val retValueTemp =schemeType match {
      case _ : vt.IntrinsicCellType =>
        // Nothing more to do!
        val isExpectedCellBool = ps.Temp(vt.CBool)
        plan.steps += ps.ConvertNativeInteger(isExpectedCellBool, isExpectedCellPred, vt.CBool.bits, false)

        isExpectedCellBool

      case recordLikeType : vt.RecordLikeType =>
        // Cast the value to its boxed form
        val recordCellTemp = ps.RecordTemp()
        plan.steps += ps.CastCellToTypeUnchecked(recordCellTemp, argumentTemp, recordLikeType.cellType)

        val classMatchedPred = ps.Temp(vt.Predicate)
        plan.steps += ps.TestRecordLikeClass(classMatchedPred, recordCellTemp, recordLikeType) 

        val classMatchedBool = ps.Temp(vt.CBool)
        plan.steps += ps.ConvertNativeInteger(classMatchedBool, classMatchedPred, vt.CBool.bits, false)

        classMatchedBool
    }

    // Return the phi'ed value
    plan.steps += ps.Return(Some(retValueTemp))

    PlannedFunction(
      signature=predicateSignature,
      namedArguments=List(("value" -> argumentTemp)),
      steps=plan.steps.toList,
      worldPtrOpt=None,
      debugContextOpt=None
    )
  }
}
