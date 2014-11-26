package io.llambda.compiler.planner.typecheck
import io.llambda

import io.llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}

object SymbolForTypePredicateProc {
  private def planTypePredicateProc(
      parentPlan : PlanWriter,
      testingType : vt.SchemeType
  ) : String = {
    val symbolHint = vt.NameForType(testingType)
      .replaceAllLiterally("<", "")
      .replaceAllLiterally(">", "") + "?"
        
    val allocedSymbol = parentPlan.allocSymbol(symbolHint)

    // We only have a single argument
    val argumentTemp = ps.CellTemp(ct.AnyCell)
    
    val plan = parentPlan.forkPlan()
        
    // Add this to the list of planned type predicates in case it's a recursive type
    plan.plannedTypePredicates += (testingType -> allocedSymbol)

    // Perform an inner type check returning a boolean result
    // Note that this is forced inline check because we pass selfSymbolOpt. The normal PlanTypeCheck entry point to the
    // type system might decide  to call this type check out-of-line which won't work because *this* is the out-of-line
    // implementation.
    val cellTemp = BoxedValue(ct.AnyCell, argumentTemp)
    val checkResult = PlanTypeCheck(cellTemp, vt.AnySchemeType, testingType, selfSymbolOpt=Some(allocedSymbol))(plan)

    val retValueTemp = checkResult.toNativePred()(plan)

    plan.steps += ps.Return(Some(retValueTemp))

    val plannedFunction = PlannedFunction(
      signature=TypePredicateProcSignature,
      namedArguments=List(("value" -> argumentTemp)),
      steps=plan.steps.toList,
      debugContextOpt=None
    )

    plan.plannedFunctions += (allocedSymbol -> plannedFunction)

    allocedSymbol
  }

  def apply(parentPlan : PlanWriter, testingType : vt.SchemeType) : String = {
    parentPlan.plannedTypePredicates.get(testingType).getOrElse {
      planTypePredicateProc(parentPlan, testingType)
    }
  }
}
