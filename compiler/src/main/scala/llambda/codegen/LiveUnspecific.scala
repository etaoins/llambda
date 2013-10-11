package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

object LiveUnspecific extends ConstantLiveValue(bt.BoxedUnspecific) {
  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    GlobalVariable("lliby_unspecific_value", PointerType(bt.BoxedUnspecific.irType))
  }
  
  // Unspecific is a unit type. It doesn't make sense to unbox them.
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = Map.empty
}
