package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.InternalCompilerErrorException

object GenSetRecordDataField {
  def apply(block : IrBlockBuilder)(recordDataIr : IrValue, generatedType : GeneratedType, recordField : vt.RecordField, newValueIr : IrValue) {
    val fieldIndices = generatedType.fieldToGepIndices(recordField)
    val fieldType = generatedType.recordLikeType.typeForField(recordField)
    val fieldIrType = ValueTypeToIr(fieldType).irType

    // Find the TBAA node
    val tbaaNode = generatedType.fieldToTbaaNode(recordField)
    val storeMetadata = Map("tbaa" -> tbaaNode)

    // Get the element pointer
    val fieldPtr = block.getelementptr("fieldPtr")(fieldIrType, recordDataIr, (0 :: fieldIndices).map(IntegerConstant(IntegerType(32), _)))

    // Perform the store
    block.store(newValueIr, fieldPtr, metadata=storeMetadata)
  }
}



