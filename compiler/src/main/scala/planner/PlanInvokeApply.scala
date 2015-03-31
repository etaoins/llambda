package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{ContextLocated, RuntimeErrorMessage, ErrorCategory, ProcedureAttribute}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

import llambda.compiler.valuetype.Implicits._

object PlanInvokeApply {
  def withTempValues(
      invokableProc : InvokableProcedure,
      fixedTemps : Seq[ps.TempValue],
      restTemps : Option[ps.TempValue]
  )(implicit plan : PlanWriter) : ResultValues = {
    val entryPointTemp = invokableProc.planEntryPoint()
    val signature = invokableProc.polySignature.upperBound

    val worldTemps = if (signature.hasWorldArg) {
      List(ps.WorldPtrValue)
    }
    else {
      Nil
    }

    val selfTemps = if (signature.hasSelfArg) {
      List(invokableProc.planSelf())
    }
    else {
      Nil
    }

    val argTemps = worldTemps ++ selfTemps ++ fixedTemps ++ restTemps

    val discardable = !invokableProc.hasSideEffects(fixedTemps.length)

    signature.returnType match {
      case vt.ReturnType.SingleValue(vt.UnitType) =>
        plan.steps += ps.Invoke(None, signature, entryPointTemp, argTemps, discardable=discardable)

        if (signature.attributes.contains(ProcedureAttribute.NoReturn)) {
          UnreachableValue
        }
        else {
          SingleValue(iv.UnitValue)
        }

      case otherType =>
        val resultTemp = ps.Temp(otherType.representationTypeOpt.get)
        plan.steps += ps.Invoke(Some(resultTemp), signature, entryPointTemp, argTemps, discardable=discardable)

        TempValueToResults(otherType, resultTemp)
    }
  }

  def withArgumentList(
      invokableProc : InvokableProcedure,
      argListValue : iv.IntermediateValue
  )(implicit plan : PlanWriter) : ResultValues = {
    val signature = invokableProc.polySignature.upperBound

    val insufficientArgsMessage = ArityRuntimeErrorMessage.insufficientArgs(invokableProc)

    val improperListMessage = RuntimeErrorMessage(
      category=ErrorCategory.Type,
      name="applyWithImproperList",
      text="Attempted application with improper list"
    )

    // Split our arguments in to fixed args and a rest arg
    val destructuredArgs = DestructureList(
      listValue=argListValue,
      memberTypes=signature.fixedArgTypes,
      insufficientLengthMessage=insufficientArgsMessage,
      improperListMessageOpt=Some(improperListMessage)
    )

    val fixedArgTemps = destructuredArgs.memberTemps
    val restArgValue = destructuredArgs.listTailValue

    val restArgTemps = signature.restArgMemberTypeOpt match {
      case Some(memberType) =>
        val requiredRestArgType = vt.UniformProperListType(memberType)
        val typeCheckedRestArg = restArgValue.toTempValue(requiredRestArgType)

        Some(typeCheckedRestArg)

      case None =>
        val tooManyArgsMessage = ArityRuntimeErrorMessage.tooManyArgs(invokableProc)
        
        // Make sure we're out of args by doing a check cast to an empty list
        restArgValue.toTempValue(vt.EmptyListType, Some(tooManyArgsMessage))
        None
    }

    PlanInvokeApply.withTempValues(invokableProc, fixedArgTemps, restArgTemps) 
  }

  def withIntermediateValues(
      invokableProc : InvokableProcedure,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : ResultValues = {
    val signature = invokableProc.polySignature.upperBound

    // Convert all the args
    val fixedTemps = args.zip(signature.fixedArgTypes) map { case ((contextLocated, arg), nativeType) =>
      plan.withContextLocation(contextLocated) {
        arg.toTempValue(nativeType)
      }
    }

    val restTemps = signature.restArgMemberTypeOpt map { memberType =>
      val restArgs = args.drop(signature.fixedArgTypes.length)

      val restArgValues = restArgs map { case (contextLocated, restValue) =>
        plan.withContextLocation(contextLocated) {
          restValue.castToSchemeType(memberType)
        }
      }

      ValuesToList(restArgValues, capturable=false).toTempValue(vt.ListElementType)
    }

    PlanInvokeApply.withTempValues(invokableProc, fixedTemps, restTemps)
  }
}

