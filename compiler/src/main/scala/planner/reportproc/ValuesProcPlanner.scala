package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ValuesProcPlanner extends ReportProcPlanner {
  def apply(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ResultValues] = (reportName, operands) match {
    case ("values", operands) =>
      // We have a specific type here!
      val operandValues = operands.map(_._2)
      Some(ResultValues(operandValues))

    case _ =>
      None
  }
}
