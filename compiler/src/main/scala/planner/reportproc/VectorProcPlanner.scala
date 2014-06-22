package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object VectorProcPlanner extends ReportProcPlanner {
  def apply(state : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("vector-length", List((_, vectorValue))) =>
      val vectorTemp = vectorValue.toTempValue(vt.VectorType)

      val resultTemp = ps.Temp(vt.UInt32)
      plan.steps += ps.LoadVectorLength(resultTemp, vectorTemp)

      Some(PlanResult(
        state=state,
        value=TempValueToIntermediate(vt.UInt32, resultTemp)
      ))

    case _ =>
      None
  }
}
