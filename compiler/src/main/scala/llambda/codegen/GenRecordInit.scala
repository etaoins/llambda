package llambda.codegen

import llambda.codegen.llvmir._
import llambda.planner.{step => ps}
import llambda.{celltype => ct}
import llambda.{valuetype => vt}

object GenRecordInit {
  case class InitializedRecord(recordCell : IrValue, recordData : IrValue)

  def apply(state : GenerationState, typeGenerator : TypeGenerator)(initStep : ps.RecordInit) : InitializedRecord  = { 
    // Declare _lliby_record_data_alloc
    val llibyRecordDataAlloc = IrFunctionDecl(
      result=IrFunction.Result(PointerType(IntegerType(8))),
      name="_lliby_record_data_alloc",
      arguments=List(
        IrFunction.Argument(IntegerType(64))
      ),
      attributes=Set(IrFunction.NoUnwind)
    )
    
    val block = state.currentBlock
    val module = state.module

    module.unlessDeclared(llibyRecordDataAlloc) {
      module.declareFunction(llibyRecordDataAlloc)
    }
    
    // Get a pointer to the new cell
    val allocation = state.liveAllocations(initStep.allocation)
    val recordCell = allocation.genTypedPointer(block)(initStep.allocIndex, ct.RecordCell) 
    
    // Get our record type information
    val recordType = initStep.recordType
    val generatedType = typeGenerator(recordType)
    val recordDataIrType = generatedType.irType 
    
    // Set the class ID
    val classIdIr = IntegerConstant(ct.RecordCell.recordClassIdIrType, generatedType.classId)
    ct.RecordCell.genStoreToRecordClassId(block)(classIdIr, recordCell)

    val uncastRecordData = generatedType.storageType match {
      case TypeDataStorage.Empty =>
        // No fields; don't bother allocating or setting the recordData pointer
        NullPointerConstant(PointerType(IntegerType(8)))

      case TypeDataStorage.Inline =>
        // Store the value inline in the cell on top of the recordData field
        // instead of going through another level of indirection
        ct.RecordCell.genPointerToRecordData(block)(recordCell)

      case TypeDataStorage.OutOfLine =>
        // Find the size of the record data
        val irSize = GenSizeOf(block)(recordDataIrType)
    
        // Allocate it using _lliby_record_data_alloc
        val voidRecordData = block.callDecl(Some("rawRecordData"))(llibyRecordDataAlloc, List(irSize)).get

        // Store the record data pointer in the new cell
        ct.RecordCell.genStoreToRecordData(block)(voidRecordData, recordCell)

        voidRecordData
    }

    val castRecordData = block.bitcastTo("castRecordData")(uncastRecordData, PointerType(recordDataIrType))

    InitializedRecord(
      recordCell=recordCell,
      recordData=castRecordData
    )
  }
}
