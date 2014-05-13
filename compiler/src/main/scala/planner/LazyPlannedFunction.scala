package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}

object LazyPlannedFunction {
  def apply(suggestedName : String, plannedFunction : PlannedFunction, selfTempOpt : Option[ps.TempValue])(implicit plan : PlanWriter) : iv.IntermediateValue = {
    // Don't emit this function until its referenced
    val symbolBlock = () => {
      val nativeSymbol = plan.allocProcedureSymbol(suggestedName)
      plan.plannedFunctions += (nativeSymbol -> plannedFunction)

      nativeSymbol
    }

    new iv.KnownProcedure(plannedFunction.signature, symbolBlock, selfTempOpt)
  }
}
