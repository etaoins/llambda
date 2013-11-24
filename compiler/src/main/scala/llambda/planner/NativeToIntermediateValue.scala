package llambda.planner

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}
import llambda.InternalCompilerErrorException

object NativeToIntermediateValue {
  def apply(nativeType : nfi.NativeType, tempValue : ps.TempValue) : iv.IntermediateValue = nativeType match {
    case nfi.CBool =>
      new iv.UnboxedBooleanValue(tempValue)

    case intType : nfi.IntType =>
      new iv.UnboxedExactIntegerValue(tempValue, intType)
    
    case fpType : nfi.FpType =>
      new iv.UnboxedInexactRationalValue(tempValue, fpType)

    case nfi.UnicodeChar =>
      new iv.UnboxedCharacterValue(tempValue)

    case nfi.Utf8CString =>
      new iv.UnboxedUtf8String(tempValue)

    case nfi.BoxedValue(boxedType) =>
      new iv.DynamicBoxedValue(boxedType.concreteTypes, boxedType, tempValue)
  }
}
