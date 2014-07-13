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
    
    case _ : ps.UnboxInexactRational =>
      val block = state.currentBlock
      ct.InexactRationalCell.genLoadFromValue(block)(boxedValue)
    
    case _ : ps.UnboxCharacter =>
      val block = state.currentBlock
      ct.CharacterCell.genLoadFromUnicodeChar(block)(boxedValue)
  }
}

