package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.ContextLocated
import llambda.compiler.planner._
import llambda.compiler.planner.{intermediatevalue => iv}

object DynamicProcPlanner extends StdlibProcPlanner {
  override def planWithResult(state: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[PlanResult] = (reportName, args) match {
    case ("make-parameter", List((_, initialValue))) =>
      val resultTemp = ps.Temp(vt.TopProcedureType)
      val initialValueTemp = initialValue.toTempValue(vt.AnySchemeType)

      plan.steps += ps.CreateParameterProc(resultTemp, initialValueTemp)

      val paramIdentity = new ParameterIdentity
      val procValue = new iv.KnownParameterProc(resultTemp, paramIdentity)

      // Log the known value of this parameter in the state
      val newState = state.copy(
        parameterValues=state.parameterValues + (paramIdentity -> KnownParameterValue(initialValue))
      )

      Some(PlanResult(
        value=procValue,
        state=newState
      ))

    case _ =>
      None
  }
}
