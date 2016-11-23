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
      args : List[et.Expr]
  )(implicit plan : PlanWriter) : Option[PlanResult] = (reportName, args) match {
    case ("apply", List(applyProcExpr)) =>
      Some(PlanApplication.planWithArgList(initialState)(applyProcExpr, iv.EmptyListValue))

    case ("apply", applyProcExpr :: applyArgExprs) =>
      val initialResult = PlanResult(initialState, SingleValue(iv.UnitValue))

      val applyArgResults = applyArgExprs.scanLeft(initialResult) { case (prevResult, applyArgExpr) =>
        PlanExpr(prevResult.state)(applyArgExpr)
      }

      val argState = applyArgResults.last.state
      val applyArgs = applyArgResults.tail.map(_.value.toSingleValue())

      val argList = ValuesToList(applyArgs.dropRight(1), applyArgs.last)
      Some(PlanApplication.planWithArgList(argState)(applyProcExpr, argList))

    case _ =>
      None
  }
}
