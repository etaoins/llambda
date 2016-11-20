package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.InternalCompilerErrorException

import llambda.compiler.valuetype.Implicits._

object TempValueToIntermediate {
  def apply(valueType : vt.ValueType, tempValue : ps.TempValue)(planConfig : PlanConfig) : iv.IntermediateValue = valueType match {
    case vt.Predicate =>
      new iv.NativePredicateValue(tempValue)

    case intType : vt.IntType =>
      new iv.NativeExactIntegerValue(tempValue, intType)
    
    case fpType : vt.FpType =>
      new iv.NativeFlonumValue(tempValue, fpType)

    case vt.UnicodeChar =>
      new iv.NativeCharValue(tempValue)

    case schemeType : vt.SchemeType =>
      val stableType = vt.StabiliseType(schemeType)
      new iv.CellValue(stableType, BoxedValue(schemeType.cellType, tempValue))
  }
}
