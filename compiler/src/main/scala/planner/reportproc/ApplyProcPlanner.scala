package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ApplyProcPlanner extends ReportProcPlanner {
  override def planWithValues(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ResultValues] = (reportName, operands) match {
    case ("apply", List((procContextLoc, procValue), (_, argListValue))) if argListValue.isDefiniteProperList =>
      // Convert to a procedure cell so we can use its trampoline
      val invokableProc = plan.withContextLocation(procContextLoc) {
        procValue.toApplicableValueForArity(operands.length).toInvokableProcedure()
      }

      Some(PlanInvokeApply.withArgumentList(invokableProc, argListValue))

    case _ =>
      None
  }
}
