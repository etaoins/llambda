package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

object GenRecordLikeInit {
  case class InitializedRecordLike(recordCell : IrValue, recordData : IrValue)

  def apply(state : GenerationState, typeGenerator : TypeGenerator)(initStep : ps.RecordLikeInit) : InitializedRecordLike  = { 
    val cellType = initStep.recordLikeType.cellType

    val block = state.currentBlock
    val module = state.module

    // Get a pointer to the new cell
    val allocation = state.liveAllocations(initStep.allocation)
    val recordCell = allocation.genTypedPointer(block)(initStep.allocIndex, cellType) 
    
    // Get our record type information
    val recordLikeType = initStep.recordLikeType
    val generatedType = typeGenerator(recordLikeType)
    val recordDataIrType = generatedType.irType 
    
    // Set the class ID
    val classIdIr = IntegerConstant(cellType.recordClassIdIrType, generatedType.classId)
    cellType.genStoreToRecordClassId(block)(classIdIr, recordCell)

    val uncastRecordData = generatedType.storageType match {
      case TypeDataStorage.Empty =>
        cellType.genStoreToDataIsInline(block)(IntegerConstant(cellType.dataIsInlineIrType, 1), recordCell)

        // No fields; don't bother allocating or setting the recordData pointer
        NullPointerConstant(PointerType(IntegerType(8)))

      case TypeDataStorage.Inline =>
        cellType.genStoreToDataIsInline(block)(IntegerConstant(cellType.dataIsInlineIrType, 1), recordCell)

        // Store the value inline in the cell on top of the recordData field
        // instead of going through another level of indirection
        cellType.genPointerToRecordData(block)(recordCell)

      case TypeDataStorage.OutOfLine =>
        cellType.genStoreToDataIsInline(block)(IntegerConstant(cellType.dataIsInlineIrType, 0), recordCell)

        // Declare _lliby_record_data_alloc
        val llibyRecordDataAlloc = IrFunctionDecl(
          result=IrFunction.Result(PointerType(IntegerType(8))),
          name="_lliby_record_data_alloc",
          arguments=List(
            IrFunction.Argument(IntegerType(64))
          ),
          attributes=Set(IrFunction.NoUnwind)
        )
    
        module.unlessDeclared(llibyRecordDataAlloc) {
          module.declareFunction(llibyRecordDataAlloc)
        }
    
        // Find the size of the record data
        val irSize = GenSizeOf(block)(recordDataIrType)
    
        // Allocate it using _lliby_record_data_alloc
        val voidRecordData = block.callDecl(Some("rawRecordData"))(llibyRecordDataAlloc, List(irSize)).get

        // Store the record data pointer in the new cell
        cellType.genStoreToRecordData(block)(voidRecordData, recordCell)

        voidRecordData
    }

    val castRecordData = block.bitcastTo("castRecordData")(uncastRecordData, PointerType(recordDataIrType))

    InitializedRecordLike(
      recordCell=recordCell,
      recordData=castRecordData
    )
  }
}
