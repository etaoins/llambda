package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{valuetype => vt}
import BoxedRecordTypeImplicits._

object GenRecordFieldSet {
  def apply(block : IrBlockBuilder)(recordDataIr : IrValue, recordType : vt.BoxedRecordType, recordField : vt.RecordField, newValueIr : IrValue) {
    val fieldIndex = recordType.indexOfField(recordField)
    val fieldIrType = ValueTypeToIr(recordField.fieldType).irType

    // Get the element pointer
    val fieldPtr = block.getelementptr("fieldPtr")(fieldIrType, recordDataIr, List(0, fieldIndex).map(IntegerConstant(IntegerType(32), _)))
  
    // Perform the store
    block.store(newValueIr, fieldPtr)
  }
}



