package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.InternalCompilerErrorException

object GenSetRecordDataField {
  def apply(block : IrBlockBuilder)(recordDataIr : IrValue, generatedType : GeneratedType, recordField : vt.RecordField, newValueIr : IrValue) {
    val fieldIndex = generatedType.fieldToStructIndex(recordField)
    val fieldIrType = ValueTypeToIr(recordField.fieldType).irType
    
    // Find the TBAA node
    val tbaaNode = generatedType.fieldToTbaaNode(recordField)
    val storeMetadata = Map("tbaa" -> tbaaNode)

    // Get the element pointer
    val fieldPtr = block.getelementptr("fieldPtr")(fieldIrType, recordDataIr, List(0, fieldIndex).map(IntegerConstant(IntegerType(32), _)))
  
    // Perform the store
    block.store(newValueIr, fieldPtr, metadata=storeMetadata)
  }
}



