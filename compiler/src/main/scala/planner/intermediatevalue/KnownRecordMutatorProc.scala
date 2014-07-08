package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class KnownRecordMutatorProc(recordType : vt.RecordType, field : vt.RecordField) extends KnownArtificialProc {
  val signature = ProcedureSignature(
    hasWorldArg=false,
    hasSelfArg=false,
    hasRestArg=false,
    fixedArgs=List(recordType, field.fieldType),
    returnType=None,
    attributes=Set()
  )

  protected val symbolHint = 
    recordType.sourceName
      .replaceAllLiterally("<", "")
      .replaceAllLiterally(">", "") + 
      "-" + field.sourceName +
      "!"
  
  def planFunction(parentPlan : PlanWriter) : PlannedFunction = {
    // Set up our arguments
    val recordCellTemp = ps.RecordTemp()
    val newValueTemp = ps.Temp(field.fieldType)

    val namedArguments = List(
      ("recordCell" -> recordCellTemp),
      ("newValue" -> newValueTemp)
    )
    
    val plan = parentPlan.forkPlan()
    
    // Extract the record data
    val recordDataTemp = ps.RecordLikeDataTemp()
    plan.steps += ps.LoadRecordLikeData(recordDataTemp, recordCellTemp, recordType) 

    // Store the new value
    plan.steps += ps.SetRecordDataField(recordDataTemp, recordType, field, newValueTemp) 
    plan.steps += ps.Return(None)

    PlannedFunction(
      signature=signature,
      namedArguments=namedArguments,
      steps=plan.steps.toList,
      worldPtrOpt=None,
      debugContextOpt=None
    )
  }

  override def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    operands match {
      case List((_, recordValue), (_, newValue)) =>
        val recordCellTemp = recordValue.toTempValue(recordType)

        val recordDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.LoadRecordLikeData(recordDataTemp, recordCellTemp, recordType) 
        
        // Read the field
        val newValueTemp = newValue.toTempValue(field.fieldType)
        plan.steps += ps.SetRecordDataField(recordDataTemp, recordType, field, newValueTemp) 

        Some(PlanResult(
          state=state,
          value=UnitValue
        ))

      case _ =>
        None
    }
  }
}
