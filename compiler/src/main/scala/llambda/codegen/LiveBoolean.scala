package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveBoolean(constantValue : Boolean) extends ConstantLiveValue(bt.BoxedBoolean) {
  override val booleanValue = constantValue

  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    if (constantValue) {
      LiveBoolean.trueIrValue
    }
    else {
      LiveBoolean.falseIrValue
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
    block.select("boxedBool")(predValue, LiveBoolean.trueIrValue, LiveBoolean.falseIrValue)
  }
}

object LiveBoolean {
  val trueIrValue = GlobalVariable("lliby_true_value", PointerType(bt.BoxedBoolean.irType))
  val falseIrValue = GlobalVariable("lliby_false_value", PointerType(bt.BoxedBoolean.irType))

  def fromConstant(value : Boolean) : ConstantLiveValue =
    new ConstantLiveBoolean(value)

  def fromUnboxed(value : IrValue) : LiveValue = 
    new UnboxedLiveBoolean(value)

  def genTruthinessCheck(initialState : GenerationState)(boxedValue : IrValue) : Option[(GenerationState, IrValue)] = {
    val block = initialState.currentBlock

    // Bitcast false constant to the expected value
    val bitcastFalseIrValue = BitcastToConstant(falseIrValue, boxedValue.irType)

    // Check if this is equal to the false singleton. If not, it's true
    val falsePred = block.icmp("boxedFalsePred")(ComparisonCond.NotEqual, None, boxedValue, bitcastFalseIrValue)
    // Sign extend to the CBool size
    val falseBool = block.zextTo("boxedFalseBool")(falsePred, IntegerType(nfi.CBool.bits))

    Some((initialState, falseBool))
  }
} 
