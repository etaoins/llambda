package llambda.codegen

import llambda.nfi

case class FirstClassTypeWithSign(
  irType : llvmir.FirstClassType,
  signed : Option[Boolean])

object NfiTypeToIrType {
  def apply(nativeType : nfi.NativeType) : FirstClassTypeWithSign = {
    nativeType match {
      case intLike : nfi.IntLikeType =>
        FirstClassTypeWithSign(llvmir.IntegerType(intLike.bits), Some(intLike.signed))

      case nfi.Float =>
        FirstClassTypeWithSign(llvmir.SingleType, None)

      case nfi.Double =>
        FirstClassTypeWithSign(llvmir.DoubleType, None)

      case nfi.BoxedValue(boxedType) =>
        FirstClassTypeWithSign(llvmir.PointerType(boxedType.irType), None)

      case nfi.Utf8CString =>
        FirstClassTypeWithSign(llvmir.PointerType(llvmir.IntegerType(8)), None)
    }
  }
}
