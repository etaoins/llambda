package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class KnownRecordMutatorProc(recordType : vt.RecordType, field : vt.RecordField) extends KnownArtificialProc(
    ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(recordType, recordType.typeForField(field)),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    ).toPolymorphic
) {
  protected val symbolHint = 
    recordType.sourceName
      .replaceAllLiterally("<", "")
      .replaceAllLiterally(">", "") + 
      "-" + field.name +
      "!"
  
  def planFunction(parentPlan : PlanWriter, allocedSymbol : String) : PlannedFunction = {
    val fieldType = recordType.typeForField(field)

    // Set up our arguments
    val recordCellTemp = ps.RecordTemp()
    val newValueTemp = ps.Temp(fieldType)

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
      signature=polySignature.upperBound,
      namedArguments=namedArguments,
      steps=plan.steps.toList,
      debugContextOpt=None
    )
  }

  override def attemptInlineApplication(state : PlannerState)(
      args : List[(ContextLocated, IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[PlanResult] = {
    args match {
      case List((_, recordValue), (_, newValue)) =>
        val recordCellTemp = recordValue.toTempValue(recordType)

        val recordDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.LoadRecordLikeData(recordDataTemp, recordCellTemp, recordType) 

        // Read the field
        val fieldType = recordType.typeForField(field)
        val newValueTemp = newValue.toTempValue(fieldType)
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
