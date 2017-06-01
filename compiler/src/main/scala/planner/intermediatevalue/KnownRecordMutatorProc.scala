package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class KnownRecordMutatorProc(recordType: vt.RecordType, field: vt.RecordField) extends KnownArtificialProc(
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

  def planFunction(parentPlan: PlanWriter): PlannedFunction = {
    // Set up our arguments
    val recordCellTemp = ps.TempValue()
    val newValueTemp = ps.TempValue()

    val namedArguments = List(
      ("recordCell" -> recordCellTemp),
      ("newValue" -> newValueTemp)
    )

    val plan = parentPlan.forkPlan()

    // Store the new value
    val fieldsToSet = List((newValueTemp -> field))
    plan.steps += ps.SetRecordLikeFields(recordCellTemp, recordType, fieldsToSet)
    plan.steps += ps.Return(None)

    PlannedFunction(
      signature=polySignature.upperBound,
      namedArguments=namedArguments,
      steps=plan.steps.toList,
      debugContextOpt=None
    )
  }

  override def attemptInlineApplication(state: PlannerState)(
      args: List[(ContextLocated, IntermediateValue)]
  )(implicit plan: PlanWriter): Option[PlanResult] = {
    args match {
      case List((_, recordValue), (_, newValue)) =>
        val recordCellTemp = recordValue.toTempValue(recordType)

        // Store the field
        val fieldType = recordType.typeForField(field)
        val newValueTemp = newValue.toTempValue(fieldType)
        val fieldsToSet = List((newValueTemp -> field))

        plan.steps += ps.SetRecordLikeFields(recordCellTemp, recordType, fieldsToSet)

        Some(PlanResult(
          state=state,
          value=UnitValue
        ))

      case _ =>
        None
    }
  }
}
