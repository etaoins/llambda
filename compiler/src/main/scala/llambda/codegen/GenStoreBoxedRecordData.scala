package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{valuetype => vt}
import llambda.{boxedtype => bt}

object GenStoreBoxedRecordData {
  def apply(block : IrBlockBuilder)(boxedRecordIr : IrValue, recordDataIrType : UserDefinedType) : IrValue = {
    // Get a pointer to the record data field
    val recordDataPtr = bt.BoxedRecord.genLoadFromRecordData(block)(boxedRecordIr)

    block.bitcastTo("castRecordData")(recordDataPtr, PointerType(recordDataIrType))
  }
}

