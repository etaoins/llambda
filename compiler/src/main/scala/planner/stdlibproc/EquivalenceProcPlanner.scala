package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._


object EquivalenceProcPlanner extends StdlibProcPlanner {
  override def planWithResult(state: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[PlanResult] = (reportName, args) match {
    case ("eqv?", List((_, val1), (_, val2))) =>
      StaticValueEqv.valuesAreEqv(val1, val2).map { staticResult =>
        PlanResult(
          state=state,
          value=iv.ConstantBooleanValue(staticResult)
        )
      } orElse {
        Some(DynamicValueEqv.valuesAreEqv(state)(val1, val2))
      }

    case ("equal?", List((_, val1), (_, val2))) =>
      StaticValueEqv.valuesAreEqual(val1, val2).map { staticResult =>
        PlanResult(
          state=state,
          value=iv.ConstantBooleanValue(staticResult)
        )
      } orElse {
        Some(DynamicValueEqv.valuesAreEqual(state)(val1, val2))
      }

    case _ =>
      None
  }
}
