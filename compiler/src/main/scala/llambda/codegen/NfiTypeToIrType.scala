package llambda.codegen

import llambda.nfi
import llambda.codegen.{boxedtype => bt}

object NfiTypeToIrType {
  def apply(nativeType : nfi.NativeType) : llvmir.FirstClassType = {
    nativeType match {
      case intLike : nfi.IntLikeType =>
        llvmir.IntegerType(intLike.bits)

      case nfi.Float =>
        llvmir.SingleType

      case nfi.Double =>
        llvmir.DoubleType

      case nfi.BoxedDatum =>
        bt.BoxedValue.llvmType

      case nfi.Utf8String =>
        llvmir.PointerType(llvmir.IntegerType(8))
    }
  }
}
