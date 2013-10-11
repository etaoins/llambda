package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveBoolean(constantValue : Boolean) extends ConstantLiveValue(bt.BoxedBoolean) {
  override val booleanValue = constantValue

  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    if (constantValue) {
      GlobalVariable("lliby_true_value", PointerType(bt.BoxedBoolean.irType))
    }
    else {
      GlobalVariable("lliby_false_value", PointerType(bt.BoxedBoolean.irType))
    }
  }
  
  // This is handled by ConstantLiveValue looking at booleanValue 
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = Map.empty
}

object LiveBoolean {
  def fromConstant(value : Boolean) : ConstantLiveValue =
    new ConstantLiveBoolean(value)
} 
