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
      // Don't evaluate applyProcExpr - it could be an inline lambda
      // We want to inline it if at all possible
      val applyArgsResult = PlanExpr(initialState)(applyArgsExpr)
      val resultValue = applyArgsResult.values.toSingleValue()

      Some(PlanApplication.planWithOperandList(applyArgsResult.state)(applyProcExpr, resultValue))

    case _ =>
      None
  }
}
