package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.codegen.llvmir._
import llambda.compiler.{valuetype => vt}

object GenRecordDataFieldSet {
  def apply(block : IrBlockBuilder)(recordDataIr : IrValue, generatedType : GeneratedType, recordField : vt.RecordField, newValueIr : IrValue) {
    val fieldIndex = generatedType.fieldToStructIndex(recordField)
    val fieldIrType = ValueTypeToIr(recordField.fieldType).irType
    
    // Find the TBAA index
    val tbaaIndex = generatedType.fieldToTbaaIndex(recordField)

    // Get the element pointer
    val fieldPtr = block.getelementptr("fieldPtr")(fieldIrType, recordDataIr, List(0, fieldIndex).map(IntegerConstant(IntegerType(32), _)))
  
    // Perform the store
    block.store(newValueIr, fieldPtr, tbaaIndex=Some(tbaaIndex))
  }
}



