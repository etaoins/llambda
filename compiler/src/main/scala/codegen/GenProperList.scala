package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.llvmir._

object GenProperList {
  // irDatumCell must be of type %datum*
  def apply(initialState : GenerationState)(irDataCells : Seq[IrValue]) : (GenerationState, IrValue) = {
    val block = initialState.currentBlock
    val emptyList = GlobalDefines.emptyListIrValue

    val bitcastEmptyList = block.bitcastTo("emptyListCast")(emptyList, PointerType(ct.DatumCell.irType))

    val (finalState, head) = irDataCells.foldRight((initialState, bitcastEmptyList)) { case (irDatumCell, (state, nextElement)) =>
      val allocation = state.currentAllocation

      block.comment(s"initializing list element")
      val (newAllocation, pairPointer) = allocation.consumeCells(block)(1, ct.PairCell)

      // Set the car
      ct.PairCell.genStoreToCar(block)(irDatumCell, pairPointer)

      // Set the cdr
      ct.PairCell.genStoreToCdr(block)(nextElement, pairPointer)

      // Pass value to the element before us
      val elementIr = block.bitcastTo(s"elementDatumCast")(pairPointer, PointerType(ct.DatumCell.irType))

      val newState = state.copy(
        currentAllocation=newAllocation
      )

      (newState, elementIr)
    }

    val headCast = block.bitcastTo("headListElementCast")(head, PointerType(ct.ListElementCell.irType))

    (finalState, headCast)
  }
}
