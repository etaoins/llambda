package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

object PlanValuePhi {
  trait Result {
    val resultValue: iv.IntermediateValue
    def planStepPhis: List[ps.ValuePhi]
  }

  private class DynamicResult(
    leftTempValue: ps.TempValue,
    rightTempValue: ps.TempValue,
    resultTemp: ps.TempValue,
    val resultValue: iv.IntermediateValue
  ) extends Result {
    def planStepPhis: List[ps.ValuePhi] =
      List(ps.ValuePhi(resultTemp, leftTempValue, rightTempValue))
  }

  private class StaticResult(val resultValue: iv.IntermediateValue) extends Result {
    def planStepPhis: List[ps.ValuePhi] = Nil
  }

  def apply(
      leftPlan: PlanWriter,
      leftValue: iv.IntermediateValue,
      rightPlan: PlanWriter,
      rightValue: iv.IntermediateValue
  ): Result =
    (leftValue, rightValue) match {
      case (leftValue: iv.ConstantValue, rightValue: iv.ConstantValue) if leftValue.isEqv(rightValue) =>
        new StaticResult(leftValue)

      case (leftInteger: iv.KnownInteger, rightInteger: iv.KnownInteger) =>
        val targetType = if (leftInteger.nativeType == rightInteger.nativeType) {
          leftInteger.nativeType
        }
        else {
          vt.Int64
        }

        val leftTempValue = leftInteger.toTempValue(targetType)(leftPlan)
        val rightTempValue = rightInteger.toTempValue(targetType)(rightPlan)

        val phiResultTemp = ps.TempValue()
        val phiPossibleValues = leftInteger.possibleValues ++ rightInteger.possibleValues

        val resultValue = new iv.NativeIntegerValue(phiResultTemp, targetType)(phiPossibleValues)

        new DynamicResult(
          leftTempValue=leftTempValue,
          rightTempValue=rightTempValue,
          resultTemp=phiResultTemp,
          resultValue=resultValue
        )

      case (leftUnboxed: iv.UnboxedValue, rightUnboxed: iv.UnboxedValue)
          if leftUnboxed.nativeType == rightUnboxed.nativeType =>
        val commonType = leftUnboxed.nativeType
        val phiResultTemp = ps.TempValue()

        val leftTempValue = leftUnboxed.toTempValue(commonType)(leftPlan)
        val rightTempValue = rightUnboxed.toTempValue(commonType)(rightPlan)

        val resultValue = TempValueToIntermediate(commonType, phiResultTemp)

        // Our types exactly match - no conversion needed!
        new DynamicResult(
          leftTempValue=leftTempValue,
          rightTempValue=rightTempValue,
          resultTemp=phiResultTemp,
          resultValue=resultValue
        )

      case (leftValue, rightValue) =>
        val phiSchemeType = leftValue.schemeType + rightValue.schemeType

        val leftTempValue = leftValue.toTempValue(phiSchemeType)(leftPlan)
        val rightTempValue = rightValue.toTempValue(phiSchemeType)(rightPlan)

        val phiResultTemp = ps.TempValue()
        val boxedValue = BoxedValue(phiSchemeType.cellType, phiResultTemp)

        new DynamicResult(
          leftTempValue=leftTempValue,
          rightTempValue=rightTempValue,
          resultTemp=phiResultTemp,
          resultValue=new iv.CellValue(phiSchemeType, boxedValue)
        )
    }
}

