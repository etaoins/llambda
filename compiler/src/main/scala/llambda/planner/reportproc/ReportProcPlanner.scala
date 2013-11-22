package llambda.planner.reportproc

import llambda.planner.{intermediatevalue => iv}
import llambda.planner._

/** Optionally replaces a call to a report procedure with plan steps */
trait ReportProcPlanner {
  def apply(initialState : PlannerState)(reportName : String, operandValues : List[iv.IntermediateValue])(implicit plan : PlanWriter) : Option[PlanResult]  
}
