package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class KnownRecordAccessorProc(recordType : vt.RecordType, field : vt.RecordField) extends KnownArtificialProc(
    ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      restArgMemberTypeOpt=None,
      fixedArgTypes=List(recordType),
      returnType=vt.ReturnType.SingleValue(field.fieldType),
      attributes=Set()
    ).toPolymorphic
) {
  protected val symbolHint =
    recordType.sourceName
      .replaceAllLiterally("<", "")
      .replaceAllLiterally(">", "") + 
      "-" + field.sourceName
        

  def planFunction(parentPlan : PlanWriter, allocedSymbol : String) : PlannedFunction = {
    val recordCellTemp = ps.RecordTemp()
    
    val plan = parentPlan.forkPlan()

    // Extract the record data
    val recordDataTemp = ps.RecordLikeDataTemp()
    plan.steps += ps.LoadRecordLikeData(recordDataTemp, recordCellTemp, recordType) 
    
    // Read the field
    val fieldValueTemp = ps.Temp(field.fieldType)
    plan.steps += ps.LoadRecordDataField(fieldValueTemp, recordDataTemp, recordType, field) 
    plan.steps += ps.Return(Some(fieldValueTemp))

    PlannedFunction(
      signature=polySignature.upperBound,
      namedArguments=List(("recordCell" -> recordCellTemp)),
      steps=plan.steps.toList,
      debugContextOpt=None
    )
  }

  override def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter) : Option[PlanResult] = {
    operands match {
      case List((_, recordValue)) =>
        val recordCellTemp = recordValue.toTempValue(recordType)

        val recordDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.LoadRecordLikeData(recordDataTemp, recordCellTemp, recordType) 
        
        // Read the field
        val fieldValueTemp = ps.Temp(field.fieldType)
        plan.steps += ps.LoadRecordDataField(fieldValueTemp, recordDataTemp, recordType, field) 

        val resultValue = TempValueToIntermediate(field.fieldType, fieldValueTemp)(plan.config)

        Some(PlanResult(
          state=state,
          values=SingleValue(resultValue)
        ))

      case _ =>
        None
    }
  }
}
