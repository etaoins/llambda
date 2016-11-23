package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{ProcedureSignature, ProcedureAttribute, StorageLocation}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.et
import llambda.compiler.{RuntimeErrorMessage, ContextLocated}

import llambda.compiler.valuetype.Implicits._

private[planner] object PlanProcedureTrampoline {
  /** Plans a trampoline for the passed procedure
    *
    * @param  trampolineSignature  The required signature for the trampoline function
    * @param  targetProc           The target procedure. The trampoline will ensure the arguments it's passed satisfy
    *                              the target procedure's signature and perform any required type conversions.
    * @param  targetProcLocOpt     Source location of the target procedure. This is used to generate a comment in the
    *                              output IR identifying the trampoline.
    */
  def apply(
      trampolineSignature : ProcedureSignature,
      targetProc : iv.InvokableProc,
      targetProcLocOpt : Option[ContextLocated] = None
  )(implicit parentPlan : PlanWriter) : PlannedFunction = {
    val inSelfTemp = ps.CellTemp(ct.ProcedureCell)
    implicit val plan = parentPlan.forkPlan()

    // Make some aliases
    val inSignature = trampolineSignature

    val updatedProc = if (targetProc.polySignature.upperBound.hasSelfArg) {
      // Load the real target proc
      val closureDataTemp = ps.RecordLikeDataTemp()
      plan.steps += ps.LoadRecordLikeData(closureDataTemp, inSelfTemp, AdapterProcType)

      val targetProcCell = ps.CellTemp(ct.ProcedureCell)
      plan.steps += ps.LoadRecordDataField(targetProcCell, closureDataTemp, AdapterProcType, AdapterProcField)

      targetProc.withSelfTemp(targetProcCell)
    }
    else {
      targetProc
    }

    val (mandatoryArgTemps, mandatoryArgValues) = inSignature.mandatoryArgTypes.map({ mandatoryArgType =>
      val mandatoryArgTemp = ps.Temp(mandatoryArgType)
      val mandatoryArgValue = TempValueToIntermediate(mandatoryArgType, mandatoryArgTemp)(plan.config)

      (mandatoryArgTemp, mandatoryArgValue)
    }).unzip

    val varArgsListType = vt.VariableArgsToListType(inSignature.optionalArgTypes, inSignature.restArgMemberTypeOpt)

    val (varArgsTempOpt, varArgsValueOpt) = if (varArgsListType == vt.EmptyListType) {
      (None, None)
    }
    else {
      val varArgsTemp = ps.CellTemp(ct.ListElementCell)
      val varArgsValue = new iv.CellValue(varArgsListType, BoxedValue(ct.ListElementCell, varArgsTemp))

      (Some(varArgsTemp), Some(varArgsValue))
    }

    val argTail = varArgsValueOpt getOrElse iv.EmptyListValue
    val argList = ValuesToList(mandatoryArgValues, argTail, capturable=false)(plan)

    // This is a bit of a hack - create a state containing our self argument as a bound variable
    val selfLoc = new StorageLocation("self")
    val procState = PlannerState(Map(selfLoc -> ImmutableValue(updatedProc)))

    // Plan the target
    val applyResult = PlanApplication.planWithArgList(procState)(et.VarRef(selfLoc), argList)(plan)

    applyResult.value match {
      case iv.UnreachableValue =>

      case reachableValue =>
        val returnTempOpt = reachableValue.toReturnTempValueOpt(trampolineSignature.returnType)(plan)
        plan.steps += ps.Return(returnTempOpt)
    }

    val irCommentOpt =
      for(targetProcLoc <- targetProcLocOpt;
          location <- targetProcLoc.locationOpt)
      yield
        s"Trampoline function for Scheme procedure defined at ${location.locationOnlyString}"

    val inNamedArguments = List(
      ("world"   -> ps.WorldPtrValue),
      ("closure" -> inSelfTemp)
    ) ++ mandatoryArgTemps.zipWithIndex.map { case (mandatoryArgTemp, index) =>
      s"mandatoryArg${index}" -> mandatoryArgTemp
    } ++ varArgsTempOpt.map { case varArgsTemp =>
      "varArgs" -> varArgsTemp
    }

    PlannedFunction(
      signature=trampolineSignature,
      namedArguments=inNamedArguments,
      steps=plan.steps.toList,
      debugContextOpt=None,
      irCommentOpt=irCommentOpt
    )
  }
}
