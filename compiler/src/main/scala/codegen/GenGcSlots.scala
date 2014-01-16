package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenGcSlots {
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

  def apply(module : IrModuleBuilder, block : IrBlockBuilder)(gcManagedValues : Set[ps.TempValue]) : Map[ps.TempValue, IrValue] = {
    // Allocate all of our GC slots
    val slotCount = gcManagedValues.size

    if (slotCount == 0) {
      Map()
    }
    else {
      val datumPointerType = PointerType(ct.DatumCell.irType)

      module.unlessDeclared(gcRootDecl) {
        module.declareFunction(gcRootDecl)
      }

      // Load and GC root each slot
      (gcManagedValues.zipWithIndex.map { case (tempValue, index) =>
        val slotGepIndices = List(IntegerConstant(IntegerType(32), index))

        // Allocate the slot
        val slotIrValue = block.alloca("gcSlot")(datumPointerType)

        // GC root the value
        val castSlotIrValue = block.bitcastTo("gcSlotCast")(slotIrValue, PointerType(PointerType(IntegerType(8))))
        block.callDecl(None)(gcRootDecl, List(castSlotIrValue, NullPointerConstant(PointerType(IntegerType(8)))))

        (tempValue, slotIrValue)
      }).toMap
    }
  }
}
