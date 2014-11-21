package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object SymbolProcPlanner extends ReportProcPlanner {
  override def planWithValue(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("string->symbol", List((_, constantString : iv.ConstantStringValue))) =>
      Some(new iv.ConstantSymbolValue(constantString.value))

    case ("symbol->string", List((_, constantSymbol : iv.ConstantSymbolValue))) =>
      Some(new iv.ConstantStringValue(constantSymbol.value))

    case _ =>
      None
  }
}
