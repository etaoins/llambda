package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object CadrProcPlanner extends ReportProcPlanner {
  override def planWithValue(initialState : PlannerState)(
      reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("car", List((located, pairValue))) =>
      plan.withContextLocation(located) {
        Some(PlanCadr.loadCar(pairValue))
      }

    case ("cdr", List((located, pairValue))) =>
      plan.withContextLocation(located) {
        Some(PlanCadr.loadCdr(pairValue))
      }

    case _ =>
      None
  }
}
