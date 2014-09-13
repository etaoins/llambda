package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}

object TempValueToResults {
  def apply(returnType : vt.ReturnType.ReturnType, resultTemp : ps.TempValue)(implicit plan : PlanWriter) : ResultValues =
    returnType match {
      case vt.ReturnType.SingleValue(valueType) =>
        val singleValue = TempValueToIntermediate(valueType, resultTemp)(plan.config)
        SingleValue(singleValue)

      case multipleValues @ vt.ReturnType.MultipleValues(valueListType) =>
        val multipleValueList = TempValueToIntermediate(valueListType, resultTemp)(plan.config)
        MultipleValues(multipleValueList)
    }
}
