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
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = (reportName, args) match {
    case ("string->symbol", List((_, iv.ConstantStringValue(constValue)))) =>
      Some(iv.ConstantSymbolValue(constValue))

    case ("symbol->string", List((_, iv.ConstantSymbolValue(constValue)))) =>
      Some(iv.ConstantStringValue(constValue))

    case _ =>
      None
  }
}
