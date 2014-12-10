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
      fixedArgTypes=List(recordType.upperBound),
      returnType=vt.ReturnType.SingleValue(recordType.storageTypeForField(field)),
      attributes=Set()
    ).toPolymorphic
) {
  protected val symbolHint =
    recordType.sourceName
      .replaceAllLiterally("<", "")
      .replaceAllLiterally(">", "") +
      "-" + field.name

  def planFunction(parentPlan : PlanWriter, allocedSymbol : String) : PlannedFunction = {
    val recordCellTemp = ps.RecordTemp()
    
    val plan = parentPlan.forkPlan()

    // Extract the record data
    val recordDataTemp = ps.RecordLikeDataTemp()
    plan.steps += ps.LoadRecordLikeData(recordDataTemp, recordCellTemp, recordType)

    // Read the field
    val fieldType = recordType.storageTypeForField(field)
    val fieldValueTemp = ps.Temp(fieldType)

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
        val upperBound = recordType.upperBound
        val recordCellTemp = recordValue.toTempValue(upperBound)

        // Do we have a speciic polymorph?
        val specificPolymorph = (recordValue.schemeType & upperBound) match {
          case specificInstance : vt.RecordTypeInstance =>
            specificInstance

          case _ =>
            upperBound
        }

        val recordDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.LoadRecordLikeData(recordDataTemp, recordCellTemp, recordType)

        val fieldType = recordType.storageTypeForField(field)
        val fieldValueTemp = ps.Temp(fieldType)

        plan.steps += ps.LoadRecordDataField(fieldValueTemp, recordDataTemp, recordType, field)

        val upperBoundValue = TempValueToIntermediate(fieldType, fieldValueTemp)(plan.config)

        // Inject the type information from our polymorph
        val resultType = specificPolymorph.schemeTypeForField(field)
        val resultValue = upperBoundValue.withSchemeType(resultType)

        Some(PlanResult(
          state=state,
          values=SingleValue(resultValue)
        ))

      case _ =>
        None
    }
  }
}
