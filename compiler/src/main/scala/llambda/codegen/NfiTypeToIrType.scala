package llambda.codegen

import llambda.nfi
import llambda.codegen.{boxedtype => bt}

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

      case nfi.BoxedDatum =>
        FirstClassTypeWithSign(bt.BoxedDatum.llvmType, None)

      case nfi.Utf8String =>
        FirstClassTypeWithSign(llvmir.PointerType(llvmir.IntegerType(8)), None)
    }
  }
}
