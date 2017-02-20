package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object BytevectorProcPlanner extends StdlibProcPlanner with StdlibProcPlannerHelpers {
  override def planWithValue(state: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
    case ("bytevector-length", List((_, iv.ConstantBytevectorValue(elements)))) =>
      Some(iv.ConstantIntegerValue(elements.length))

    case ("bytevector-length", List((located, bytevectorValue))) =>
      val bytevectorTemp = plan.withContextLocation(located) {
        bytevectorValue.toTempValue(vt.BytevectorType)
      }

      val resultTemp = ps.Temp(vt.Int64)
      plan.steps += ps.LoadBytevectorLength(resultTemp, bytevectorTemp)

      Some(TempValueToIntermediate(vt.Int64, resultTemp))

    case ("bytevector-u8-ref", List((_, iv.ConstantBytevectorValue(elements)), (_, iv.ConstantIntegerValue(index)))) =>
      assertIndexValid("(bytevector-ref)", elements.size, index)

      Some(new iv.ConstantIntegerValue(elements(index.toInt)))

    case _ =>
      None
  }
}
