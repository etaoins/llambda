package llambda.codegen

import llambda.nfi

case class SignedFirstClassType(
  irType : llvmir.FirstClassType,
  signed : Option[Boolean])

object NfiTypeToIrType {
  def apply(nativeType : nfi.NativeType) : SignedFirstClassType = {
    nativeType match {
      case intLike : nfi.IntLikeType =>
        SignedFirstClassType(llvmir.IntegerType(intLike.bits), Some(intLike.signed))

      case nfi.Float =>
        SignedFirstClassType(llvmir.FloatType, None)

      case nfi.Double =>
        SignedFirstClassType(llvmir.DoubleType, None)

      case nfi.BoxedValue(boxedType) =>
        SignedFirstClassType(llvmir.PointerType(boxedType.irType), None)

      case nfi.Utf8CString =>
        SignedFirstClassType(llvmir.PointerType(llvmir.IntegerType(8)), None)
    }
  }
}
