package llambda.codegen

import llambda.nfi
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

object NativeToLiveValue {
  def apply(state : GenerationState)(nativeType : nfi.NativeType, nativeValue : IrValue) : (GenerationState, LiveValue) = nativeType match {
    case nfi.BoxedValue(boxedType) =>
      (state, new BoxedLiveValue(boxedType, nativeValue))

    case nfi.CBool =>
      (state, LiveBoolean.fromUnboxed(nativeValue))

    case intType : nfi.IntType =>
      (state, LiveExactInteger.fromUnboxed(nativeValue, intType))

    case fpType : nfi.FpType =>
      (state, LiveInexactRational.fromUnboxed(nativeValue, fpType))
    
    case nfi.UnicodeChar =>
      (state, LiveCharacter.fromUnboxed(nativeValue))

  }
}
