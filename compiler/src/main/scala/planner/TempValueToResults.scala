package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.ReturnType

object TempValueToResults {
  def apply(returnType : ReturnType.ReturnType, resultTemp : ps.TempValue)(implicit plan : PlanWriter) : ResultValues =
    returnType match {
      case ReturnType.SingleValue(valueType) =>
        val singleValue = TempValueToIntermediate(valueType, resultTemp)(plan.config)
        SingleValue(singleValue)

      case multipleValues @ ReturnType.MultipleValues(valueListType) =>
        val multipleValueList = TempValueToIntermediate(valueListType, resultTemp)(plan.config)
        MultipleValues(multipleValueList)
    }
}
