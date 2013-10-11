package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

class ConstantLiveString(constantValue : String) extends ConstantLiveStringLike(constantValue, bt.BoxedString) {
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = {
    case nfi.Utf8CString =>
      StringConstant(constantValue)
  }
}

object LiveString {
  def fromConstant(value : String) : ConstantLiveValue =
    new ConstantLiveString(value)
} 
