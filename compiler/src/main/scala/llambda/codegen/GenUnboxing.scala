package llambda.codegen

import llambda.InternalCompilerErrorException

import llambda.planner.{step => ps}
import llambda.codegen.llvmir._
import llambda.{boxedtype => bt}

object GenUnboxing {
  def apply(state : GenerationState)(unboxStep : ps.UnboxValue, boxedValue : IrValue) : IrValue = unboxStep match {
    case _ : ps.UnboxAsTruthy =>
      val block = state.currentBlock

      // Bitcast false constant to the expected value
      val bitcastFalseIrValue = BitcastToConstant(GlobalDefines.falseIrValue, boxedValue.irType)

      // Check if this is equal to the false singleton. If not, it's true
      block.icmp("truthyPred")(ComparisonCond.NotEqual, None, boxedValue, bitcastFalseIrValue)

    case _ : ps.UnboxExactInteger =>
      val block = state.currentBlock
      bt.BoxedExactInteger.genLoadFromValue(block)(boxedValue)
    
    case _ : ps.UnboxInexactRational =>
      val block = state.currentBlock
      bt.BoxedInexactRational.genLoadFromValue(block)(boxedValue)
    
    case _ : ps.UnboxCharacter =>
      val block = state.currentBlock
      bt.BoxedCharacter.genLoadFromUnicodeChar(block)(boxedValue)
    
    case _ : ps.UnboxStringAsUtf8 =>
      val block = state.currentBlock
      bt.BoxedString.genLoadFromUtf8Data(block)(boxedValue)
  }
}

