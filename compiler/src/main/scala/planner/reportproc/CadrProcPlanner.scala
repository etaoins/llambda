package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object CadrProcPlanner extends ReportProcPlanner {
  def apply(initialState : PlannerState)(
      reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ResultValues] = (reportName, operands) match {
    case ("car", List((located, pairValue))) =>
      Some(SingleValue(
        PlanCadr.loadCar(located, pairValue)
      ))

    case ("cdr", List((located, pairValue))) =>
      Some(SingleValue(
        PlanCadr.loadCdr(located, pairValue)
      ))

    case _ =>
      None
  }
}
