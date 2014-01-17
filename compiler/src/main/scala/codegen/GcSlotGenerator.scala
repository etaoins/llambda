package io.llambda.compiler.codegen
import io.llambda

import collection.mutable

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._

class GcSlotGenerator(module : IrModuleBuilder, entryBlock : IrEntryBlockBuilder) extends {
  private val gcRootDecl = {
    IrFunctionDecl(
      result=IrFunction.Result(VoidType),
      name="llvm.gcroot",
      arguments=List(
        IrFunction.Argument(PointerType(PointerType(IntegerType(8)))),
        IrFunction.Argument(PointerType(IntegerType(8)))
      )
    )
  }
  
  private val allocatedSlots = new mutable.HashMap[ps.TempValue, IrValue]

  module.unlessDeclared(gcRootDecl) {
    module.declareFunction(gcRootDecl)
  }

  def apply(tempValue : ps.TempValue) : IrValue = 
    allocatedSlots(tempValue)

  def getOrElseCreate(tempValue : ps.TempValue, irType : FirstClassType) : IrValue = {
    allocatedSlots.getOrElseUpdate(tempValue, {
      // Allocate the slot
      // This stores a pointer to the actual value that the GC can update when
      // things move around
      val slotIrValue = entryBlock.alloca("gcSlot")(irType)

      // GC root the value
      val castSlotIrValue = entryBlock.bitcastTo("gcSlotCast")(slotIrValue, PointerType(PointerType(IntegerType(8))))
      entryBlock.callDecl(None)(gcRootDecl, List(castSlotIrValue, NullPointerConstant(PointerType(IntegerType(8)))))

      slotIrValue
    })
  }

  def finalize(nextBlock : IrChildBlockBuilder) {
    entryBlock.uncondBranch(nextBlock)
  }
}
