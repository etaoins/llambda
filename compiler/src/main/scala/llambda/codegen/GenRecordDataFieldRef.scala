package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{valuetype => vt}

object GenRecordDataFieldRef {
  def apply(block : IrBlockBuilder)(recordDataIr : IrValue, generatedType : GeneratedType, recordField : vt.RecordField) : IrValue = {
    val fieldIndex = generatedType.fieldToStructIndex(recordField)
    val fieldIrType = ValueTypeToIr(recordField.fieldType).irType

    // Find the TBAA index
    val tbaaIndex = generatedType.fieldToTbaaIndex(recordField)

    // Get the element pointer
    val fieldPtr = block.getelementptr("fieldPtr")(fieldIrType, recordDataIr, List(0, fieldIndex).map(IntegerConstant(IntegerType(32), _)))
  
    // Perform the load
    block.load("loadedField")(fieldPtr, tbaaIndex=Some(tbaaIndex))
  }
}



