package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}
import llambda.compiler.planner.{step => ps}

class KnownRecordConstructorProc(recordType : vt.RecordType, initializedFields : List[vt.RecordField]) extends KnownArtificialProc(
    ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      restArgMemberTypeOpt=None,
      fixedArgTypes=initializedFields.map(recordType.storageTypeForField),
      returnType=vt.ReturnType.SingleValue(recordType.upperBound),
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
      val fieldType = recordType.storageTypeForField(field)
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
    val dataTemp = ps.RecordLikeDataTemp()

    plan.steps += ps.InitRecordLike(cellTemp, dataTemp, recordType, isUndefined=false)

    // Set all our fields
    for(field <- recordType.fieldsWithInherited) {
      val fieldTemp = fieldToTempValue.getOrElse(field, {
        val fieldType = recordType.storageTypeForField(field)
        UnitValue.toTempValue(fieldType)(plan)
      })

      plan.steps += ps.SetRecordDataField(dataTemp, recordType, field, fieldTemp)
    }

    // Return the record
    plan.steps += ps.Return(Some(cellTemp))

    PlannedFunction(
      signature=polySignature.upperBound,
      namedArguments=namedArguments,
      steps=plan.steps.toList,
      debugContextOpt=None
    )
  }

  override def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter) : Option[PlanResult] = {
    // Try to determine our polymorphic type from our initialised fields
    val resolvedVars = operands.map(_._2).zip(initializedFields).foldLeft(pm.ResolveTypeVars.Result()) {
      case (resolved, (operandValue, field)) =>
        resolved ++ pm.ResolveTypeVars(recordType.typeVars.toSet, field.typeTemplate, operandValue.schemeType)
    }

    val reconciledVars = pm.ReconcileTypeVars(recordType.typeVars.toSet, resolvedVars, fixApplicable=true)
    val typeInstance = vt.RecordTypeInstance(reconciledVars, recordType)

    if (operands.length != initializedFields.length) {
      // Not the right number of operands
      return None
    }

    // Convert our fields to the correct values
    val fieldToTempValue = (operands.zip(initializedFields) map { case ((operandLoc, operandValue), field) =>
      // Note that we need the upper bound type as we physically store the record using the upper bound
      val fieldType = recordType.storageTypeForField(field)
      val tempValue = plan.withContextLocation(operandLoc) {
        operandValue.toTempValue(fieldType)
      }

      (field -> tempValue)
    }).toMap

    // Build the record value
    val cellTemp = ps.RecordTemp()
    val dataTemp = ps.RecordLikeDataTemp()

    plan.steps += ps.InitRecordLike(cellTemp, dataTemp, recordType, isUndefined=false)

    for(field <- recordType.fieldsWithInherited) {
      val fieldTemp = fieldToTempValue.getOrElse(field, {
        val fieldType = recordType.storageTypeForField(field)
        UnitValue.toTempValue(fieldType)(plan)
      })

      plan.steps += ps.SetRecordDataField(dataTemp, recordType, field, fieldTemp)
    }

    val resultValue = TempValueToIntermediate(typeInstance, cellTemp)(plan.config)

    Some(PlanResult(
      state=state,
      values=SingleValue(resultValue)
    ))
  }
}
