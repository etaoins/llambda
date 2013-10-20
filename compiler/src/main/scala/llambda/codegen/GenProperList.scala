package llambda.codegen

import llambda.InternalCompilerErrorException
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.codegen.llvmir.IrFunction._

object GenProperList {
  // boxedData must be of type %datum*
  def apply(initialState : GenerationState)(irBoxedData : Seq[IrValue]) : (GenerationState, IrValue) ={
    val listLength = irBoxedData.length
    val emptyList = LiveEmptyList.genBoxedConstant()

    // This is a noop on listLength == 0
    val (state, allocation) = GenConsAllocation(initialState)(listLength) 

    val block = state.currentBlock
    val bitcastEmptyList = block.bitcastTo("emptyListCast")(emptyList, PointerType(bt.BoxedDatum.irType))

    val head = irBoxedData.zipWithIndex.foldRight(bitcastEmptyList) { case ((irBoxedDatum, index), nextElement) =>
      block.comment(s"initializing list element ${index}")

      val pairPointer = allocation.genTypedPointer(state)(index, bt.BoxedPair)

      // Set the car
      bt.BoxedPair.genStoreToCar(block)(irBoxedDatum, pairPointer)

      // Set the cdr
      bt.BoxedPair.genStoreToCdr(block)(nextElement, pairPointer)

      // Pass value to the element before us
      block.bitcastTo(s"element${index}DatumCast")(pairPointer, PointerType(bt.BoxedDatum.irType))
    }

    val headCast = block.bitcastTo("headListElementCast")(head, PointerType(bt.BoxedListElement.irType))

    (state, headCast)
  }
}
