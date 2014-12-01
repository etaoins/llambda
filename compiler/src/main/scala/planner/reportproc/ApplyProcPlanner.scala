package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.et
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ApplyProcPlanner extends ReportProcPlanner {
  override def planFromExprs(initialState : PlannerState)(
      reportName : String,
      operands : List[et.Expr]
  )(implicit plan : PlanWriter) : Option[PlanResult] = (reportName, operands) match {
    case ("apply", List(applyProcExpr, applyArgsExpr)) =>
      // Plan this on a separate plan so we don't double plan applyArgsExpr if we fail
      val staticApplyPlan = plan.forkPlan()

      // Don't evaluate applyProcExpr - it could be an inline lambda
      // We want to inline it if at all possible
      val applyArgsResult = PlanExpr(initialState)(applyArgsExpr)(staticApplyPlan)
      val resultValue = applyArgsResult.values.toSingleValue()

      resultValue match {
        case knownListElement : iv.KnownListElement =>
          knownListElement.toValueListOpt map { argValues =>
            // We statically know our arguments!
            val locatedArgValues = argValues.map((applyArgsExpr, _))

            plan.steps ++= staticApplyPlan.steps

            PlanApplication.planWithOperandValues(applyArgsResult.state)(
              applyProcExpr,
              locatedArgValues
            )
          }

        case _ =>
          // Not a known list
          None
      }

    case _ =>
      None
  }

  override def planWithValues(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[ResultValues] = (reportName, operands) match {
    case ("apply", List((procContextLoc, procValue), (_, argListValue))) if argListValue.isDefiniteProperList =>
      // Convert to a procedure cell so we can use its trampoline
      val invokableProc = plan.withContextLocation(procContextLoc) {
        procValue.toInvokableProcedure()
      }

      Some(PlanInvokeApply.withArgumentList(invokableProc, argListValue))

    case _ =>
      None
  }
}
