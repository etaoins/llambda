package llambda.codegen

import llambda.InternalCompilerErrorException
import llambda.{celltype => ct}
import llambda.codegen.llvmir._
import llambda.codegen.llvmir.IrFunction._

object GenProperList {
  // irDatumCell must be of type %datum*
  def apply(block : IrBlockBuilder)(allocation : GenConsAllocation.ConsAllocation, allocBase : Int, irDataCells : Seq[IrValue]) : IrValue ={
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
