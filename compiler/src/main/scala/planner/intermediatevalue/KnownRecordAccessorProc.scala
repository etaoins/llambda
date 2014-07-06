package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class KnownRecordAccessorProc(signature : ProcedureSignature, nativeSymbol : String, recordType : vt.RecordType, field : vt.RecordField) extends KnownProc(signature, nativeSymbol, None) {
  override def restoreFromClosure(valueType : vt.ValueType, varTemp : ps.TempValue) : IntermediateValue = {
    new KnownRecordAccessorProc(signature, nativeSymbol, recordType, field)
  }

  override def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    operands match {
      case List((_, recordValue)) =>
        val recordCellTemp = recordValue.toTempValue(recordType)

        val recordDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.LoadRecordLikeData(recordDataTemp, recordCellTemp, recordType) 
        
        // Read the field
        val fieldValueTemp = ps.Temp(field.fieldType)
        plan.steps += ps.LoadRecordDataField(fieldValueTemp, recordDataTemp, recordType, field) 

        Some(PlanResult(
          state=state,
          value=TempValueToIntermediate(field.fieldType, fieldValueTemp)
        ))

      case _ =>
        None
    }
  }
}
