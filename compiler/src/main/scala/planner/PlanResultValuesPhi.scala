package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ReturnType
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
  
object PlanResultValuesPhi {
  case class Result(
    leftTempValue : ps.TempValue,
    rightTempValue : ps.TempValue,
    resultTemp : ps.TempValue,
    resultValues : ResultValues
  )

  def apply(
      leftPlan : PlanWriter,
      leftValues : ResultValues,
      rightPlan : PlanWriter,
      rightValues : ResultValues
  )(implicit worldPtr : ps.WorldPtrValue) : Result =
    (leftValues, rightValues) match {
      case (SingleValue(leftUnboxed : iv.NativeValue), SingleValue(rightUnboxed : iv.NativeValue))
          if leftUnboxed.nativeType == rightUnboxed.nativeType =>
        val commonType = leftUnboxed.nativeType
        val phiResultTemp = ps.Temp(commonType)
        
        Result(
          leftTempValue=leftUnboxed.tempValue,
          rightTempValue=rightUnboxed.tempValue,
          resultTemp=phiResultTemp,
          resultValues=SingleValue(leftUnboxed.withNewTempValue(phiResultTemp))
        )

      case (SingleValue(leftValue), SingleValue(rightValue)) =>
        val phiSchemeType = leftValue.schemeType + rightValue.schemeType

        // Our types exactly match - no conversion needed!
        val leftTempValue = leftValue.toTempValue(phiSchemeType)(leftPlan, worldPtr)
        val rightTempValue = rightValue.toTempValue(phiSchemeType)(rightPlan, worldPtr)

        // If we're constants on both sides we don't need to be GC managed
        val isGcManaged = leftTempValue.isGcManaged || rightTempValue.isGcManaged

        val phiResultTemp = new ps.TempValue(isGcManaged)

        val boxedValue = BoxedValue(phiSchemeType.cellType, phiResultTemp)

        Result(
          leftTempValue=leftTempValue,
          rightTempValue=rightTempValue,
          resultTemp=phiResultTemp,
          resultValues=SingleValue(new iv.CellValue(phiSchemeType, boxedValue))
        )

      case _ =>
        // Construct value lists to deal with this
        val leftValueList = leftValues.toMultipleValueList()(leftPlan, worldPtr)
        val rightValueList = rightValues.toMultipleValueList()(rightPlan, worldPtr)

        val phiResultListType = leftValueList.schemeType + rightValueList.schemeType
        val phiResultTemp = ps.Temp(phiResultListType)

        val phiResultList = TempValueToIntermediate(phiResultListType, phiResultTemp)(leftPlan.config)

        Result(
          leftTempValue=leftValueList.toTempValue(phiResultListType)(leftPlan, worldPtr),
          rightTempValue=rightValueList.toTempValue(phiResultListType)(rightPlan, worldPtr),
          resultTemp=phiResultTemp,
          resultValues=MultipleValues(phiResultList)
        )
    }
}

