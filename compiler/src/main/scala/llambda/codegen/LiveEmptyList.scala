package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

object LiveEmptyList extends ConstantLiveValue(bt.BoxedEmptyList) {
  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    GlobalVariable("lliby_empty_list_value", PointerType(bt.BoxedEmptyList.irType))
  }
  
  // This is a structural type. Unboxing it doesn't make sense.
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = Map.empty
}
