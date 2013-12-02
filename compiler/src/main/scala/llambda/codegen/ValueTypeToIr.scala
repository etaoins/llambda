package llambda.codegen

import llambda.nfi
import llambda.{valuetype => vt}
import llambda.{boxedtype => bt}

case class SignedFirstClassType(
  irType : llvmir.FirstClassType,
  signed : Option[Boolean])

object ValueTypeToIr {
  def apply(valueType : vt.ValueType) : SignedFirstClassType = valueType match {
    case vt.ScalarType(intLike : nfi.IntLikeType) =>
      SignedFirstClassType(llvmir.IntegerType(intLike.bits), Some(intLike.signed))

    case vt.ScalarType(nfi.Float) =>
      SignedFirstClassType(llvmir.FloatType, None)

    case vt.ScalarType(nfi.Double) =>
      SignedFirstClassType(llvmir.DoubleType, None)

    case vt.ScalarType(nfi.Utf8CString) =>
      SignedFirstClassType(llvmir.PointerType(llvmir.IntegerType(8)), None)

    case vt.BoxedIntrinsicType(boxedType) =>
      SignedFirstClassType(llvmir.PointerType(boxedType.irType), None)

    case _ : vt.BoxedRecordType =>
      // All boxed records have the same IR type. Their data is cast to the 
      // correct type on demand
      apply(vt.BoxedIntrinsicType(bt.BoxedRecord))
  }
}
