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

      case (leftSpecific @ SpecificValues(leftBoxed, leftTypes),
            rightSpecific @ SpecificValues(rightBoxed, rightTypes)
      ) if (leftTypes.size == rightTypes.size) =>
        val phiResultTypes = leftTypes.zip(rightTypes) map { case (leftType, rightType) =>
          leftType + rightType
        }

        // If the size of the list is the same then the head of the list must be of the same type
        val resultCellType = leftBoxed.cellType 

        val phiResultTemp = ps.CellTemp(resultCellType)
        val phiResultBoxed = BoxedValue(resultCellType, phiResultTemp) 

        Result(
          leftTempValue=leftBoxed.tempValue,
          rightTempValue=rightBoxed.tempValue,
          resultTemp=phiResultTemp,
          resultValues=SpecificValues(phiResultBoxed, phiResultTypes)
        )

      case _ =>
        // Fallback
        val leftTempValue = leftValues.toReturnTempValue(ReturnType.ArbitraryValues)(leftPlan, worldPtr).get
        val rightTempValue = rightValues.toReturnTempValue(ReturnType.ArbitraryValues)(rightPlan, worldPtr).get

        val isGcManaged = leftTempValue.isGcManaged || rightTempValue.isGcManaged

        val resultCellType = ReturnType.ArbitraryValues.representationType.cellType

        val phiResultTemp = new ps.TempValue(isGcManaged)
        val phiResultBoxed = BoxedValue(resultCellType, phiResultTemp)

        Result(
          leftTempValue=leftTempValue,
          rightTempValue=rightTempValue,
          resultTemp=phiResultTemp,
          resultValues=ArbitraryValues(phiResultBoxed)
        )
    }
}

