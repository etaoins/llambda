package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object CadrProcPlanner extends StdlibProcPlanner {
  override def planWithValue(initialState: PlannerState)(
      reportName: String, args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
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
