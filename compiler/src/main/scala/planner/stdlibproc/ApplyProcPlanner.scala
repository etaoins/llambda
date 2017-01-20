package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.et
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ApplyProcPlanner extends StdlibProcPlanner {
  override def planFromExprs(initialState: PlannerState)(
      reportName: String,
      args: List[et.Expr]
  )(implicit plan: PlanWriter): Option[PlanResult] = (reportName, args) match {
    case ("apply", List(applyProcExpr)) =>
      Some(PlanApplication.planWithArgList(initialState)(applyProcExpr, iv.EmptyListValue))

    case ("apply", applyProcExpr :: applyArgExprs) =>
      val initialResult = PlanResult(initialState, iv.UnitValue)

      val applyArgResults = applyArgExprs.scanLeft(initialResult) { case (prevResult, applyArgExpr) =>
        PlanExpr(prevResult.state)(applyArgExpr)
      }

      val argState = applyArgResults.last.state
      val applyArgs = applyArgResults.tail.map(_.value)

      val argList = ValuesToList(applyArgs.dropRight(1), applyArgs.last)
      Some(PlanApplication.planWithArgList(argState)(applyProcExpr, argList))

    case _ =>
      None
  }
}
