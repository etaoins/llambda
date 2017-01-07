package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenBoxing {
  private def allocateBox(state: GenerationState)(
    boxStep: ps.AllocatingBoxValue,
    cellType: ct.ConcreteCellType
  )(initialiser: (IrBlockBuilder, IrValue) => Unit): (GenerationState, IrValue) = {
    val block = state.currentBlock
    val allocation = state.currentAllocation

    val (newAllocation, boxedCell) = if (boxStep.stackAllocate) {
      (allocation, GenStackAllocation(block)(cellType))
    }
    else {
      allocation.consumeCells(block)(1, cellType)
    }

    // Do type-specific initialisation
    initialiser(block, boxedCell)

    val newState = state.copy(
      currentAllocation=newAllocation
    )

    (newState, boxedCell)
  }

  def apply(state: GenerationState)(boxStep: ps.BoxValue, nativeValue: IrValue): (GenerationState, IrValue) = boxStep match {
    case _: ps.BoxBoolean =>
      val irValue = state.currentBlock.select("boxedBool")(
        nativeValue,
        GlobalDefines.trueIrValue,
        GlobalDefines.falseIrValue
      )

      (state, irValue)

    case boxInt: ps.BoxInteger =>
      allocateBox(state)(boxInt, ct.IntegerCell) { (block, boxedIntCell) =>
        ct.IntegerCell.genStoreToValue(block)(nativeValue, boxedIntCell)
      }

    case boxFlonum: ps.BoxFlonum =>
      allocateBox(state)(boxFlonum, ct.FlonumCell) { (block, boxedFlonumCell) =>
        ct.FlonumCell.genStoreToValue(block)(nativeValue, boxedFlonumCell)
      }

    case boxChar: ps.BoxChar =>
      allocateBox(state)(boxChar, ct.CharCell) { (block, boxedCharCell) =>
        ct.CharCell.genStoreToUnicodeChar(block)(nativeValue, boxedCharCell)
      }
  }
}


