package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}
import llambda.compiler.ast

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

      val int32Type = IntegerType(32)
      val rangeMetadata = RangeMetadata(int32Type, (ast.CharLiteral.firstCodePoint, ast.CharLiteral.lastCodePoint + 1))
      val loadMetadata = Map("range" -> rangeMetadata)

      ct.CharCell.genLoadFromUnicodeChar(block)(boxedValue, metadata=loadMetadata)
  }
}

