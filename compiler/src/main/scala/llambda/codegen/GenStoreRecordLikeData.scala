package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{valuetype => vt}
import llambda.{celltype => ct}

import llambda.InternalCompilerErrorException

object GenStoreRecordLikeData {
  def apply(block : IrBlockBuilder)(recordCellIr : IrValue, generatedType : GeneratedType) : IrValue = {
    val cellType = generatedType.recordLikeType.cellType
    val recordDataIrType = generatedType.irType 

    val uncastRecordData = generatedType.storageType match {
      case TypeDataStorage.Empty =>
        throw new InternalCompilerErrorException("Attempted to get record data pointer for empty record")

      case TypeDataStorage.Inline =>
        // Our data is inline; return a pointer to the record data pointer itself
        cellType.genPointerToRecordData(block)(recordCellIr)

      case TypeDataStorage.OutOfLine =>
        // Our data is out-of-line; dereference the record data pointer
        cellType.genLoadFromRecordData(block)(recordCellIr)
    }

    block.bitcastTo("castRecordData")(uncastRecordData, PointerType(recordDataIrType))
  }
}

