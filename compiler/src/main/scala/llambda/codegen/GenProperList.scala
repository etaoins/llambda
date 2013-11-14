package llambda.codegen

import llambda.InternalCompilerErrorException
import llambda.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.codegen.llvmir.IrFunction._

object GenProperList {
  // boxedData must be of type %datum*
  def apply(block : IrBlockBuilder)(allocation : GenConsAllocation.ConsAllocation, allocBase : Int, irBoxedData : Seq[IrValue]) : IrValue ={
    val listLength = irBoxedData.length
    val emptyList = GlobalDefines.emptyListIrValue

    val bitcastEmptyList = block.bitcastTo("emptyListCast")(emptyList, PointerType(bt.BoxedDatum.irType))

    val head = irBoxedData.zipWithIndex.foldRight(bitcastEmptyList) { case ((irBoxedDatum, index), nextElement) =>
      block.comment(s"initializing list element ${index}")

      val pairPointer = allocation.genTypedPointer(block)(allocBase + index, bt.BoxedPair)

      // Set the car
      bt.BoxedPair.genStoreToCar(block)(irBoxedDatum, pairPointer)

      // Set the cdr
      bt.BoxedPair.genStoreToCdr(block)(nextElement, pairPointer)

      // Pass value to the element before us
      block.bitcastTo(s"element${index}DatumCast")(pairPointer, PointerType(bt.BoxedDatum.irType))
    }

    block.bitcastTo("headListElementCast")(head, PointerType(bt.BoxedListElement.irType))
  }
}
