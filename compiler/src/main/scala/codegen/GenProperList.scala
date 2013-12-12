package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.{celltype => ct}
import llambda.compiler.codegen.llvmir._
import llambda.compiler.codegen.llvmir.IrFunction._

object GenProperList {
  // irDatumCell must be of type %datum*
  def apply(block : IrBlockBuilder)(allocation : GenCellAllocation.CellAllocation, allocBase : Int, irDataCells : Seq[IrValue]) : IrValue ={
    val listLength = irDataCells.length
    val emptyList = GlobalDefines.emptyListIrValue

    val bitcastEmptyList = block.bitcastTo("emptyListCast")(emptyList, PointerType(ct.DatumCell.irType))

    val head = irDataCells.zipWithIndex.foldRight(bitcastEmptyList) { case ((irDatumCell, index), nextElement) =>
      block.comment(s"initializing list element ${index}")

      val pairPointer = allocation.genTypedPointer(block)(allocBase + index, ct.PairCell)

      // Set the car
      ct.PairCell.genStoreToCar(block)(irDatumCell, pairPointer)

      // Set the cdr
      ct.PairCell.genStoreToCdr(block)(nextElement, pairPointer)

      // Pass value to the element before us
      block.bitcastTo(s"element${index}DatumCast")(pairPointer, PointerType(ct.DatumCell.irType))
    }

    block.bitcastTo("headListElementCast")(head, PointerType(ct.ListElementCell.irType))
  }
}
