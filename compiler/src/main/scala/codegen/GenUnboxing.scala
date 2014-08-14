package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenUnboxing {
  def apply(state : GenerationState)(unboxStep : ps.UnboxValue, boxedValue : IrValue) : IrValue = unboxStep match {
    case _ : ps.UnboxExactInteger =>
      val block = state.currentBlock
      ct.ExactIntegerCell.genLoadFromValue(block)(boxedValue)
    
    case _ : ps.UnboxFlonum =>
      val block = state.currentBlock
      ct.FlonumCell.genLoadFromValue(block)(boxedValue)
    
    case _ : ps.UnboxChar =>
      val block = state.currentBlock
      ct.CharCell.genLoadFromUnicodeChar(block)(boxedValue)
  }
}

