package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{valuetype => vt}

object GenLoadRecordDataField {
  def apply(block : IrBlockBuilder)(recordDataIr : IrValue, generatedType : GeneratedType, recordField : vt.RecordField) : IrValue = {
    val fieldIndex = generatedType.fieldToStructIndex(recordField)
    val fieldIrType = ValueTypeToIr(recordField.fieldType).irType

    // Find the TBAA node
    val tbaaNode = generatedType.fieldToTbaaNode(recordField)

    // Get the element pointer
    val fieldPtr = block.getelementptr("fieldPtr")(fieldIrType, recordDataIr, List(0, fieldIndex).map(IntegerConstant(IntegerType(32), _)))
  
    // Perform the load
    block.load("loadedField")(fieldPtr, metadata=Map("tbaa" -> tbaaNode))
  }
}



