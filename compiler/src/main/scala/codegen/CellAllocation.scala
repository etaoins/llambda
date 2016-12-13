package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.{celltype => ct}
import llambda.llvmir._

class CellAllocation(basePointer: IrValue, currentOffset: Int, totalSize: Int) {
  private val cellType = UserDefinedType("cell")

  // Returns true if this allocation is empty
  def isEmpty: Boolean =
    remainingCells == 0

  def remainingCells: Int =
    totalSize - currentOffset

  def consumeCells(block: IrBlockBuilder)(count: Int, asType: ct.ConcreteCellType): (CellAllocation, IrValue) = {
    if ((currentOffset + count) > totalSize) {
      throw new InternalCompilerErrorException("Attempted to access cell past end of allocation")
    }

    // We have to do this on %cell because the target type might be the wrong size
    val indexValue = IntegerConstant(IntegerType(32), currentOffset)
    val cellPointer = block.getelementptr(s"cell${currentOffset}Ptr")(cellType, basePointer, List(indexValue))

    // Cast to the destination type
    val pointerName = s"cell${currentOffset}${asType.llvmName.capitalize}Ptr"
    val typedPointer = block.bitcastTo(pointerName)(cellPointer, PointerType(asType.irType))

    // Set its garbage state
    val garbageState = IntegerConstant(ct.AnyCell.gcStateIrType, 0)
    asType.genStoreToGcState(block)(garbageState, typedPointer)

    // Set its type
    val typeId = IntegerConstant(ct.AnyCell.typeIdIrType, asType.typeId)
    asType.genStoreToTypeId(block)(typeId, typedPointer)

    // Return the typed pointer and new allocation
    val newAllocation = new CellAllocation(basePointer, currentOffset + count, totalSize)

    (newAllocation, typedPointer)
  }
}

object EmptyCellAllocation {
  def apply(): CellAllocation = {
    // Any attempt to use this will immediately fail
    new CellAllocation(NullPointerConstant(PointerType(ct.AnyCell.irType)), 0, 0)
  }
}
