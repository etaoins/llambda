package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{valuetype => vt}
import llambda.{celltype => ct}

import llambda.InternalCompilerErrorException

object GenStoreRecordCellData {
  def apply(block : IrBlockBuilder)(recordCellIr : IrValue, generatedType : GeneratedType) : IrValue = {
    val recordDataIrType = generatedType.irType 

    val uncastRecordData = generatedType.storageType match {
      case TypeDataStorage.Empty =>
        throw new InternalCompilerErrorException("Attempted to get record data pointer for empty record")

      case TypeDataStorage.Inline =>
        // Our data is inline; return a pointer to the record data pointer itself
        ct.RecordCell.genPointerToRecordData(block)(recordCellIr)

      case TypeDataStorage.OutOfLine =>
        // Our data is out-of-line; dereference the record data pointer
        ct.RecordCell.genLoadFromRecordData(block)(recordCellIr)
    }

    block.bitcastTo("castRecordData")(uncastRecordData, PointerType(recordDataIrType))
  }
}

