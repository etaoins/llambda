package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.llvmir._


/** Allocates a cell on the stack
  *
  * This bypasses the garbage collector. Callers must ensure the stack allocated cell has the correct lifetime.
  */
object GenStackAllocation {
  def apply(block: IrBlockBuilder)(cellType: ct.ConcreteCellType): IrValue = {
    val allocationName = "stack" + cellType.llvmName.capitalize

    val typedPointer = block.alloca(allocationName)(cellType.irType)

    val garbageState = IntegerConstant(ct.AnyCell.gcStateIrType, ct.GarbageState.StackAllocatedCell)
    cellType.genStoreToGcState(block)(garbageState, typedPointer)

    val typeId = IntegerConstant(ct.AnyCell.typeIdIrType, cellType.typeId)
    cellType.genStoreToTypeId(block)(typeId, typedPointer)

    typedPointer
  }
}

