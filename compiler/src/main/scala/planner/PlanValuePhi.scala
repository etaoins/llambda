package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

object PlanValuePhi {
  case class Result(
    leftTempValue: ps.TempValue,
    rightTempValue: ps.TempValue,
    resultTemp: ps.TempValue,
    resultValue: iv.IntermediateValue
  )

  def apply(
      leftPlan: PlanWriter,
      leftValue: iv.IntermediateValue,
      rightPlan: PlanWriter,
      rightValue: iv.IntermediateValue
  ): Result =
    (leftValue, rightValue) match {
      case (leftUnboxed: iv.UnboxedValue, rightUnboxed: iv.UnboxedValue)
          if leftUnboxed.nativeType == rightUnboxed.nativeType =>
        val commonType = leftUnboxed.nativeType
        val phiResultTemp = ps.Temp(commonType)

        val leftTempValue = leftUnboxed.toTempValue(commonType)(leftPlan)
        val rightTempValue = rightUnboxed.toTempValue(commonType)(rightPlan)

        val resultValue = TempValueToIntermediate(commonType, phiResultTemp)

        // Our types exactly match - no conversion needed!
        Result(
          leftTempValue=leftTempValue,
          rightTempValue=rightTempValue,
          resultTemp=phiResultTemp,
          resultValue=resultValue
        )

      case (leftValue, rightValue) =>
        val phiSchemeType = leftValue.schemeType + rightValue.schemeType

        val leftTempValue = leftValue.toTempValue(phiSchemeType)(leftPlan)
        val rightTempValue = rightValue.toTempValue(phiSchemeType)(rightPlan)

        // If we're constants on both sides we don't need to be GC managed
        val isGcManaged = leftTempValue.isGcManaged || rightTempValue.isGcManaged

        val phiResultTemp = new ps.TempValue(isGcManaged)

        val boxedValue = BoxedValue(phiSchemeType.cellType, phiResultTemp)

        Result(
          leftTempValue=leftTempValue,
          rightTempValue=rightTempValue,
          resultTemp=phiResultTemp,
          resultValue=new iv.CellValue(phiSchemeType, boxedValue)
        )
    }
}

