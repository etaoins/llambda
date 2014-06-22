package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}

object PlanTypePredicate {
  def apply(testingType : vt.SchemeType)(implicit parentPlan : PlanWriter) : PlannedFunction =  {
    // Determine our signature
    val predicateSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      hasRestArg=false,
      // We must be able to take any data type without erroring out
      fixedArgs=List(vt.AnySchemeType),
      returnType=Some(vt.CBool),
      attributes=Set()
    )
    
    // We only have a single argument
    val argumentTemp = ps.CellTemp(ct.DatumCell)
    
    val plan = parentPlan.forkPlan()

    // Perform an inner type check returning a boolean result
    val retValueTemp = PlanTypeCheck(argumentTemp, vt.AnySchemeType, testingType, 
      isTypeBuilder={ isTypePlan =>
        val trueBool = ps.Temp(vt.CBool)
        isTypePlan.steps += ps.CreateNativeInteger(trueBool, 1, vt.CBool.bits)

        trueBool
      },
      isNotTypeBuilder={ isNotTypePlan =>
        val falseBool = ps.Temp(vt.CBool)
        isNotTypePlan.steps += ps.CreateNativeInteger(falseBool, 0, vt.CBool.bits)

        falseBool
      }
    )(plan)

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
