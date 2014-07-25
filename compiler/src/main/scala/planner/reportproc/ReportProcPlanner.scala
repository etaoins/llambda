package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._

/** Optionally replaces a call to a report procedure with plan steps */
trait ReportProcPlanner {
  def apply(initialState : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue]  
}
