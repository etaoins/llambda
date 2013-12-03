package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{valuetype => vt}
import llambda.{celltype => ct}

object GenStoreRecordCellData {
  def apply(block : IrBlockBuilder)(recordCellIr : IrValue, recordDataIrType : UserDefinedType) : IrValue = {
    // Get a pointer to the record data field
    val recordDataPtr = ct.RecordCell.genLoadFromRecordData(block)(recordCellIr)

    block.bitcastTo("castRecordData")(recordDataPtr, PointerType(recordDataIrType))
  }
}

