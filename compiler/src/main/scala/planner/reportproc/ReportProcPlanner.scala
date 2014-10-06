package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._

/** Optionally replaces a call to a report procedure with plan steps */
abstract class ReportProcPlanner {
  def planWithResult(initialState : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    planWithValues(initialState)(reportName, operands) map { values =>
      PlanResult(
        state=initialState,
        values=values
      )
    }
  }

  def planWithValues(initialState : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ResultValues] = {
    planWithValue(initialState)(reportName, operands) map { value =>
      SingleValue(value)
    }
  }

  def planWithValue(initialState : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] =
    throw new Exception("At least one ReportProcPlanner method must be implemented")
}
