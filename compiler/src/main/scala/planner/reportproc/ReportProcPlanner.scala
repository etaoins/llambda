package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

/** Optionally replaces a call to a report procedure with plan steps */
trait ReportProcPlanner {
  def apply(initialState : PlannerState)(reportName : String, operandValues : List[iv.IntermediateValue])(implicit plan : PlanWriter) : Option[PlanResult]  
}
