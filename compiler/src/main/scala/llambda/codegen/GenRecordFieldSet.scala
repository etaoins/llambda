package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{valuetype => vt}
import RecordCellTypeImplicits._

object GenRecordFieldSet {
  def apply(block : IrBlockBuilder)(recordDataIr : IrValue, generatedType : GeneratedType, recordField : vt.RecordField, newValueIr : IrValue) {
    val fieldIndex = generatedType.recordType.indexOfField(recordField)
    val fieldIrType = ValueTypeToIr(recordField.fieldType).irType
    
    // Find the TBAA index
    val tbaaIndex = generatedType.fieldToTbaaIndex(recordField)

    // Get the element pointer
    val fieldPtr = block.getelementptr("fieldPtr")(fieldIrType, recordDataIr, List(0, fieldIndex).map(IntegerConstant(IntegerType(32), _)))
  
    // Perform the store
    block.store(newValueIr, fieldPtr, tbaaIndex=Some(tbaaIndex))
  }
}



