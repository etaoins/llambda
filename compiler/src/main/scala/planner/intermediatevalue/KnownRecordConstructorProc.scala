package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class KnownRecordConstructorProc(signature : ProcedureSignature, nativeSymbol : String, recordType : vt.RecordType, initializedFields : List[vt.RecordField]) extends KnownProc(signature, nativeSymbol, None) {
  override def restoreFromClosure(valueType : vt.ValueType, varTemp : ps.TempValue) : IntermediateValue = {
    new KnownRecordConstructorProc(signature, nativeSymbol, recordType, initializedFields)
  }

  override def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    if (operands.length != initializedFields.length) {
      // Not the right number of operands
      return None
    }

    // Convert our fields to the correct values
    val fieldToTempValue = (operands.map(_._2).zip(initializedFields) map { case (operandValue, field) =>
      (field -> operandValue.toTempValue(field.fieldType))
    }).toMap

    // Build the record value
    val cellTemp = ps.RecordTemp()
    val dataTemp = ps.RecordLikeDataTemp()

    plan.steps += ps.InitRecordLike(cellTemp, dataTemp, recordType, isUndefined=false)

    for(field <- recordType.fields) {
      val fieldTemp = fieldToTempValue.getOrElse(field, {
        UnitValue.toTempValue(field.fieldType)(plan, worldPtr)
      })
        
      plan.steps += ps.SetRecordDataField(dataTemp, recordType, field, fieldTemp)
    }

    Some(PlanResult(
      state=state,
      value=new CellValue(recordType, recordType.cellType, cellTemp) 
    ))
  }
}
