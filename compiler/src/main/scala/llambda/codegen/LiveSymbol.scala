package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveSymbol(module : IrModuleBuilder)(constantValue : String) extends ConstantLiveStringLike(module)(constantValue, bt.BoxedSymbol) {
  // We could implicitly convert this to string but requiring the use of
  // (symbol->string) first seems safer
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = Map.empty
}

object LiveSymbol {
  def fromConstant(module : IrModuleBuilder)(value : String) : ConstantLiveValue =
    new ConstantLiveSymbol(module)(value)
} 
