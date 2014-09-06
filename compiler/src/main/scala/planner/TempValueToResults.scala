package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.ReturnType

object TempValueToResults {
  def apply(returnType : ReturnType.ReturnType, resultTemp : ps.TempValue)(implicit plan : PlanWriter) : ResultValues =
    returnType match {
      case ReturnType.SingleValue(valueType) =>
        val singleValue = TempValueToIntermediate(valueType, resultTemp)(plan.config)
        new SingleValue(singleValue)

      case specificValues @ ReturnType.SpecificValues(valueTypes) =>
        val resultCellType = specificValues.representationType.cellType
        val resultBoxed = BoxedValue(resultCellType, resultTemp)

        new SpecificValues(resultBoxed, valueTypes)
        
      case ReturnType.ArbitraryValues =>
        val resultCellType = ReturnType.ArbitraryValues.representationType.cellType
        val resultBoxed = BoxedValue(resultCellType, resultTemp)

        new ArbitraryValues(resultBoxed)
    }
}
