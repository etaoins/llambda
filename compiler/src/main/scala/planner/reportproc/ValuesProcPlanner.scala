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
      operands : List[et.Expr]
  )(implicit plan : PlanWriter) : Option[PlanResult] = (reportName, operands) match {
    case ("call-with-values", List(producerExpr, consumerExpr))  =>
      // Call the producer, possibly while inlining
      val producerResult = plan.withContextLocation(producerExpr) {
        PlanApplication(initialState)(producerExpr, Nil)
      }

      val resultValue = producerResult.values.toMultipleValueList()
      Some(PlanApplication.planWithOperandList(producerResult.state)(consumerExpr, resultValue))

    case _ =>
      None
  }

  override def planWithValues(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[ResultValues] = (reportName, operands) match {
    case ("values", operands) =>
      // We have a specific type here!
      val operandValues = operands.map(_._2)
      Some(ResultValues(operandValues))

    case _ =>
      None
  }
}
