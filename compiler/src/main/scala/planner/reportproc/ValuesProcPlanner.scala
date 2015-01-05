package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.et
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ValuesProcPlanner extends ReportProcPlanner {
  override def planFromExprs(initialState : PlannerState)(
      reportName : String,
      args : List[et.Expr]
  )(implicit plan : PlanWriter) : Option[PlanResult] = (reportName, args) match {
    case ("call-with-values", List(producerExpr, consumerExpr))  =>
      // Call the producer, possibly while inlining
      val producerResult = plan.withContextLocation(producerExpr) {
        PlanApplication(initialState)(producerExpr, Nil)
      }

      val resultValue = producerResult.values.toMultipleValueList()
      Some(PlanApplication.planWithArgList(producerResult.state)(consumerExpr, resultValue))

    case _ =>
      None
  }

  override def planWithValues(state : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[ResultValues] = (reportName, args) match {
    case ("values", args) =>
      // We have a specific type here!
      val argValues = args.map(_._2)
      Some(ResultValues(argValues))

    case _ =>
      None
  }
}
