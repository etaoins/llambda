package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.InternalCompilerErrorException

object GenSetRecordDataField {
  def apply(block : IrBlockBuilder)(recordDataIr : IrValue, generatedType : GeneratedType, recordField : vt.RecordField, newValueIrOpt : Option[IrValue]) {
    val fieldIndex = generatedType.fieldToStructIndex(recordField)
    val fieldIrType = ValueTypeToIr(recordField.fieldType).irType
    
    // Find the TBAA index
    val tbaaIndex = generatedType.fieldToTbaaIndex(recordField)

    // Get the element pointer
    val fieldPtr = block.getelementptr("fieldPtr")(fieldIrType, recordDataIr, List(0, fieldIndex).map(IntegerConstant(IntegerType(32), _)))
  
    newValueIrOpt match {
      case Some(newValueIr) =>
        // Perform the store
        block.store(newValueIr, fieldPtr, tbaaIndex=Some(tbaaIndex))

      case None =>
        val pointerFieldIrType = fieldIrType match {
          case pointerType : PointerType =>
           pointerType

          case _ =>
            throw new InternalCompilerErrorException("Attempted to set non-pointer field as undefined")
        }

        block.store(NullPointerConstant(pointerFieldIrType), fieldPtr, tbaaIndex=Some(tbaaIndex))
    }
  }
}



