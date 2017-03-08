package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.InternalCompilerErrorException


object GenInitRecordLike {
  import Implicits._

  def apply(
      state: GenerationState,
      generatedTypes: Map[vt.RecordLikeType, GeneratedType]
  )(initStep: ps.InitRecordLikeStep): (GenerationState, IrValue) = {
    val cellType = initStep.recordLikeType.cellType

    val block = state.currentBlock
    val module = block.function.module

    // Get our record type information
    val recordLikeType = initStep.recordLikeType
    val generatedType = generatedTypes(recordLikeType)
    val recordDataIrType = generatedType.irType

    // Get a pointer to the new cell
    val allocation = state.currentAllocation

    val (newAllocation, recordCell) = if (initStep.stackAllocate) {
      if (generatedType.storageType == TypeDataStorage.OutOfLine) {
        // This would require a destructor which we do not support for stack allocated values
        throw new InternalCompilerErrorException("Attempted to allocate a record on the stack with out-of-line storage")
      }

      (allocation, GenStackAllocation(block)(cellType))
    }
    else {
      allocation.consumeCells(block)(1, cellType)
    }

    // Set the class ID
    val classIdIr = IntegerConstant(cellType.recordClassIdIrType, generatedType.classId)
    cellType.genStoreToRecordClassId(block)(classIdIr, recordCell)

    val castRecordData = generatedType.storageType match {
      case TypeDataStorage.Empty =>
        cellType.genStoreToDataIsInline(block)(IntegerConstant(cellType.dataIsInlineIrType, 1), recordCell)

        // No fields; don't bother allocating or setting the recordData pointer
        NullPointerConstant(PointerType(recordDataIrType))

      case TypeDataStorage.Inline =>
        cellType.genStoreToDataIsInline(block)(IntegerConstant(cellType.dataIsInlineIrType, 1), recordCell)

        // Store the value inline in the cell on top of the recordData field instead of going through another level of
        // indirection
        val uncastRecordData = cellType.genPointerToRecordData(block)(recordCell)
        block.bitcastTo("castRecordData")(uncastRecordData, PointerType(recordDataIrType))

      case TypeDataStorage.OutOfLine =>
        val recordDataAllocDecl = RuntimeFunctions.recordDataAlloc

        cellType.genStoreToDataIsInline(block)(IntegerConstant(cellType.dataIsInlineIrType, 0), recordCell)

        // Declare llcore_record_data_alloc
        module.unlessDeclared(recordDataAllocDecl) {
          module.declareFunction(recordDataAllocDecl)
        }

        // Find the size of the record data
        val irSize = GenSizeOf(recordDataIrType)

        // Allocate it using llcore_record_data_alloc
        val voidRecordData = block.callDecl(Some("rawRecordData"))(recordDataAllocDecl, List(irSize)).get

        // Store the record data pointer in the new cell
        cellType.genStoreToRecordData(block)(voidRecordData, recordCell)

        block.bitcastTo("castRecordData")(voidRecordData, PointerType(recordDataIrType))
    }

    if (cellType == ct.RecordCell) {
      val isUndefinedIr = IntegerConstant(cellType.isUndefinedIrType, if (initStep.isUndefined) 1 else 0)
      cellType.genStoreToIsUndefined(block)(isUndefinedIr, recordCell)
    }

    val irValueToRecordField = initStep.fieldValues.toList.map { case (field, valueTemp) =>
      (state.liveTemps(valueTemp).irValue, field)
    }

    GenSetRecordLikeFields(block)(castRecordData, generatedType, irValueToRecordField)

    val newState = state.copy(
      currentAllocation=newAllocation
    )

    (newState, recordCell)
  }
}
