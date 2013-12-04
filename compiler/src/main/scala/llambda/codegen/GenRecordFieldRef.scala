package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{valuetype => vt}
import RecordCellTypeImplicits._

object GenRecordFieldRef {
  def apply(block : IrBlockBuilder)(recordDataIr : IrValue, generatedRecordType : GeneratedRecordType, recordField : vt.RecordField) : IrValue = {
    val fieldIndex = generatedRecordType.recordType.indexOfField(recordField)
    val fieldIrType = ValueTypeToIr(recordField.fieldType).irType

    // Find the TBAA index
    val tbaaIndex = generatedRecordType.fieldToTbaaIndex(recordField)

    // Get the element pointer
    val fieldPtr = block.getelementptr("fieldPtr")(fieldIrType, recordDataIr, List(0, fieldIndex).map(IntegerConstant(IntegerType(32), _)))
  
    // Perform the load
    block.load("loadedField")(fieldPtr, tbaaIndex=Some(tbaaIndex))
  }
}



