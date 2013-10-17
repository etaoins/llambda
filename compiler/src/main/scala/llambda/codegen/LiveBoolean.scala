package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveBoolean(module : IrModuleBuilder)(constantValue : Boolean) extends ConstantLiveValue(bt.BoxedBoolean) {
  override val booleanValue = constantValue

  def genBoxedConstant() : IrConstant = {
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
  override def genTruthyPredicate(state : GenerationState) : IrValue = {
    val block = state.currentBlock

    // Cast the value to i1
    block.truncTo("truthyPred")(unboxedValue, IntegerType(1))
  }

  def genBoxedValue(state : GenerationState) : IrValue = {
    val predValue = genTruthyPredicate(state)

    // Use a select to pick the correct instance
    state.currentBlock.select("boxedBool")(predValue, LiveBoolean.trueIrValue, LiveBoolean.falseIrValue)
  }
}

object LiveBoolean {
  val trueIrValue = GlobalVariable("lliby_true_value", PointerType(bt.BoxedBoolean.irType))
  val falseIrValue = GlobalVariable("lliby_false_value", PointerType(bt.BoxedBoolean.irType))

  def fromConstant(module : IrModuleBuilder)(value : Boolean) : ConstantLiveValue =
    new ConstantLiveBoolean(module)(value)

  def fromUnboxed(value : IrValue) : LiveValue = 
    new UnboxedLiveBoolean(value)

  def genTruthyCheck(initialState : GenerationState)(boxedValue : IrValue) : IrValue = {
    val block = initialState.currentBlock

    // Bitcast false constant to the expected value
    val bitcastFalseIrValue = BitcastToConstant(falseIrValue, boxedValue.irType)

    // Check if this is equal to the false singleton. If not, it's true
    block.icmp("truthyPred")(ComparisonCond.NotEqual, None, boxedValue, bitcastFalseIrValue)
  }
} 
