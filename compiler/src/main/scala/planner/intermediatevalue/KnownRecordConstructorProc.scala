package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class KnownRecordConstructorProc(recordType : vt.RecordType, initializedFields : List[vt.RecordField]) extends KnownArtificialProc(
    ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      restArgMemberTypeOpt=None,
      fixedArgTypes=initializedFields.map(recordType.typeForField),
      returnType=vt.ReturnType.SingleValue(recordType),
      attributes=Set()
    ).toPolymorphic
){
  protected val symbolHint =
    recordType.sourceName
      .replaceAllLiterally("<", "")
      .replaceAllLiterally(">", "")

  def planFunction(parentPlan : PlanWriter, allocedSymbol : String) : PlannedFunction = {
    val plan = parentPlan.forkPlan()

    val fieldToTempValue = (initializedFields.map { field =>
      val fieldType = recordType.typeForField(field)
      (field, ps.Temp(fieldType))
    }).toMap

    // Get unique argument names
    val argumentUniquer = new SourceNameUniquer
    argumentUniquer.reserve("world")

    val namedArguments = ("world" -> ps.WorldPtrValue) ::
      (initializedFields.map { case field =>
        (argumentUniquer(field.name) -> fieldToTempValue(field))
      }).toList

    // Initialize the record
    val cellTemp = ps.RecordTemp()

    // Find our field values
    val allFieldValues = (recordType.fieldsWithInherited map { field =>
      val fieldTemp = fieldToTempValue.getOrElse(field, {
        val fieldType = recordType.typeForField(field)
        UnitValue.toTempValue(fieldType)(plan)
      })

      field -> fieldTemp
    }).toMap

    plan.steps += ps.InitRecord(cellTemp, recordType, allFieldValues, isUndefined=false)

    // Return the record
    plan.steps += ps.Return(Some(cellTemp))

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
    if (args.length != initializedFields.length) {
      // Not the right number of args
      return None
    }

    // Convert our fields to the correct values
    val argFieldToValue = (args.map(_._2).zip(initializedFields) map { case (argValue, field) =>
      (field -> argValue)
    }).toMap

    // Include any implicitly initialised values
    val fieldValues = (recordType.fieldsWithInherited.map { field =>
      field -> argFieldToValue.getOrElse(field, UnitValue)
    }).toMap

    val resultValue = ValuesToRecord(recordType, fieldValues, isUndefined=false)

    Some(PlanResult(
      state=state,
      values=SingleValue(resultValue)
    ))
  }
}
