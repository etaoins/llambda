package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.OutOfBoundsException
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

import llambda.compiler.valuetype.Implicits._

object VectorProcPlanner extends ReportProcPlanner {
  def apply(state : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("vector-ref", List((_, constantVector : iv.ConstantVectorValue), (_, constantInt : iv.ConstantExactIntegerValue))) =>
      val index = constantInt.value

      if ((index < 0) || (index >= constantVector.elements.length)) {
        throw new OutOfBoundsException(
          plan.activeContextLocated,
          s"Vector index ${index} out of bounds"
        )
      }

      Some(constantVector.elements(index.toInt))

    case ("vector-length", List((_, constantVector : iv.ConstantVectorValue))) =>
      Some(new iv.ConstantExactIntegerValue(constantVector.elements.length))
      
    case ("vector-length", List((located, vectorValue))) =>
      val vectorTemp = plan.withContextLocation(located) {
        vectorValue.toTempValue(vt.VectorOfType(vt.AnySchemeType))
      }

      val resultTemp = ps.Temp(vt.UInt32)
      plan.steps += ps.LoadVectorLength(resultTemp, vectorTemp)

      Some(TempValueToIntermediate(vt.UInt32, resultTemp)(plan.config))

    case _ =>
      None
  }
}
