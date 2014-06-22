package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.InternalCompilerErrorException

object TempValueToIntermediate {
  def apply(valueType : vt.ValueType, tempValue : ps.TempValue) : iv.IntermediateValue = valueType match {
    case vt.CBool =>
      new iv.NativeBooleanValue(tempValue)

    case intType : vt.IntType =>
      new iv.NativeExactIntegerValue(tempValue, intType)
    
    case fpType : vt.FpType =>
      new iv.NativeInexactRationalValue(tempValue, fpType)

    case vt.UnicodeChar =>
      new iv.NativeCharacterValue(tempValue)

    case schemeType : vt.SchemeType =>
      new iv.CellValue(schemeType, schemeType.cellType, tempValue)

    case _ : vt.ClosureType =>
      // "Closures" are mostly a fiction to re-use the machinery of records.
      // They should be treated as procedures
      throw new InternalCompilerErrorException("Closure used as a first-class type")
  }
}
