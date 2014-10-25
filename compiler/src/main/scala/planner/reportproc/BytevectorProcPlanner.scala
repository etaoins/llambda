package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object BytevectorProcPlanner extends ReportProcPlanner {
  override def planWithValue(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("bytevector-length", List((_, constantBytevector : iv.ConstantBytevectorValue))) =>
      Some(new iv.ConstantExactIntegerValue(constantBytevector.elements.length))

    case ("bytevector-length", List((located, bytevectorValue))) =>
      val bytevectorTemp = plan.withContextLocation(located) {
        bytevectorValue.toTempValue(vt.BytevectorType)
      }

      val resultTemp = ps.Temp(vt.UInt32)
      plan.steps += ps.LoadBytevectorLength(resultTemp, bytevectorTemp)

      Some(TempValueToIntermediate(vt.UInt32, resultTemp)(plan.config))

    case _ =>
      None
  }
}
