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

private class UnboxedLiveBoolean(unboxedValue : IrValue) extends UnboxedLiveValue(bt.BoxedBoolean, nfi.CBool, unboxedValue) {
  def genBoxedValue(state : GenerationState) : IrValue = {
    val block = state.currentBlock

    // Cast the value to i1
    val predValue = block.truncTo("pred")(unboxedValue, IntegerType(1))

    // Use a select to pick the correct instance
    val trueValue = GlobalVariable("lliby_true_value", PointerType(bt.BoxedBoolean.irType))
    val falseValue = GlobalVariable("lliby_false_value", PointerType(bt.BoxedBoolean.irType))

    block.select("boxedBool")(predValue, trueValue, falseValue)
  }
}

object LiveBoolean {
  def fromConstant(value : Boolean) : ConstantLiveValue =
    new ConstantLiveBoolean(value)

  def fromUnboxed(value : IrValue) : LiveValue = 
    new UnboxedLiveBoolean(value)
} 
