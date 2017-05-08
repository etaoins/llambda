package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._

import llambda.compiler.InternalCompilerErrorException

object GenLoadRecordLikeData {
  def apply(block: IrBlockBuilder)(
      recordIr: IrValue,
      generatedType: GeneratedType,
      forceStorageTypeOpt: Option[TypeDataStorage.Value] = None
  ): IrValue = {
    val cellType = generatedType.recordLikeType.cellType
    val recordDataIrType = generatedType.irType

    val storageType = forceStorageTypeOpt getOrElse generatedType.storageType

    storageType match {
      case TypeDataStorage.Empty =>
        throw new InternalCompilerErrorException("Attempted to get record data pointer for empty record")

      case TypeDataStorage.Inline if forceStorageTypeOpt.isDefined || !generatedType.hasOutOfLineDescendants =>
        // Our data is always inline; return a pointer to the record data pointer itself
        val uncastRecordData = cellType.genPointerToRecordData(block)(recordIr)
        block.bitcastTo("castRecordData")(uncastRecordData, PointerType(recordDataIrType))

      case TypeDataStorage.Inline =>
        // This could be inline or out-of-line; determine dynamically

        val loadMetadata = Map("range" -> RangeMetadata(cellType.dataIsInlineIrType, (0, 2)))
        val dataIsInlineBoolIr = cellType.genLoadFromDataIsInline(block)(recordIr, loadMetadata)

        val dataIsInlinePredIr = block.truncTo("dataIsInlinePred")(dataIsInlineBoolIr, IntegerType(1))

        // Call ourselves recursively to get each pointer
        val inlineDataIr = GenLoadRecordLikeData(block)(recordIr, generatedType, Some(TypeDataStorage.Inline))
        val oolDataIr = GenLoadRecordLikeData(block)(recordIr, generatedType, Some(TypeDataStorage.OutOfLine))

        // Select the correct value
        block.select("selectedRecordData")(dataIsInlinePredIr, inlineDataIr, oolDataIr)

      case TypeDataStorage.OutOfLine =>
        // Our data is out-of-line; dereference the record data pointer

        // While the data pointed to by this pointer may change the pointer itself cannot
        val loadMetadata = Map("invariant.load" -> GlobalDefines.emptyMetadataNode)

        val uncastRecordData = cellType.genLoadFromRecordData(block)(recordIr, loadMetadata)
        block.bitcastTo("castRecordData")(uncastRecordData, PointerType(recordDataIrType))
    }
  }
}

