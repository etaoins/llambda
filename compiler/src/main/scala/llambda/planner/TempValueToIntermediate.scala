package llambda.planner

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}
import llambda.InternalCompilerErrorException
import llambda.NotImplementedException

object TempValueToIntermediate {
  def apply(valueType : vt.ValueType, tempValue : ps.TempValue) : iv.IntermediateValue = valueType match {
    case vt.ScalarType(nfi.CBool) =>
      new iv.UnboxedBooleanValue(tempValue)

    case vt.ScalarType(intType : nfi.IntType) =>
      new iv.UnboxedExactIntegerValue(tempValue, intType)
    
    case vt.ScalarType(fpType : nfi.FpType) =>
      new iv.UnboxedInexactRationalValue(tempValue, fpType)

    case vt.ScalarType(nfi.UnicodeChar) =>
      new iv.UnboxedCharacterValue(tempValue)

    case vt.ScalarType(nfi.Utf8CString) =>
      new iv.UnboxedUtf8String(tempValue)

    case vt.BoxedIntrinsicType(boxedType) =>
      new iv.BoxedIntrinsicValue(boxedType.concreteTypes, boxedType, tempValue)

    case recordType : vt.BoxedRecordType =>
      new iv.BoxedRecordValue(recordType, tempValue)
  }
}
