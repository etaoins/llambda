package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.ContextLocated
import llambda.compiler.planner._
import llambda.compiler.planner.{intermediatevalue => iv}

object DynamicProcPlanner extends StdlibProcPlanner {
  override def planWithValue(state: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
    case ("make-parameter", List(initialValue)) =>
      val resultTemp = ps.Temp(vt.TopProcedureType)
      val initialValueTemp = initialValue._2.toTempValue(vt.AnySchemeType)

      plan.steps += ps.CreateParameterProc(resultTemp, initialValueTemp)

      Some(new iv.KnownParameterProc(resultTemp, initialValue._2, initialValueInScope=true))

    case _ =>
      None
  }
}
