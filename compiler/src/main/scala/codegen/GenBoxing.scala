package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenBoxing {
  def apply(state: GenerationState)(boxStep: ps.BoxValue, nativeValue: IrValue): (GenerationState, IrValue) = boxStep match {
    case _: ps.BoxBoolean =>
      val irValue = state.currentBlock.select("boxedBool")(
        nativeValue,
        GlobalDefines.trueIrValue,
        GlobalDefines.falseIrValue
      )

      (state, irValue)

    case _: ps.BoxInteger =>
      val block = state.currentBlock
      val allocation = state.currentAllocation

      val (newAllocation, boxedIntCons) = allocation.consumeCells(block)(1, ct.IntegerCell)
      ct.IntegerCell.genStoreToValue(block)(nativeValue, boxedIntCons)

      val newState = state.copy(
        currentAllocation=newAllocation
      )

      (newState, boxedIntCons)

    case _: ps.BoxFlonum =>
      val block = state.currentBlock
      val allocation = state.currentAllocation

      val (newAllocation, boxedFlonumCons) = allocation.consumeCells(block)(1, ct.FlonumCell)
      ct.FlonumCell.genStoreToValue(block)(nativeValue, boxedFlonumCons)

      val newState = state.copy(
        currentAllocation=newAllocation
      )

      (newState, boxedFlonumCons)

    case _: ps.BoxChar =>
      val block = state.currentBlock
      val allocation = state.currentAllocation

      val (newAllocation, boxedCharCons) = allocation.consumeCells(block)(1, ct.CharCell)
      ct.CharCell.genStoreToUnicodeChar(block)(nativeValue, boxedCharCons)

      val newState = state.copy(
        currentAllocation=newAllocation
      )

      (newState, boxedCharCons)
  }
}


