package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{ContextLocated, RuntimeErrorMessage, ErrorCategory, ProcedureAttribute}
import llambda.compiler.{valuetype => vt}

object PlanInvokeApply {
  private def withTempValues(
      invokableProc: iv.InvokableProc,
      fixedTemps: Seq[ps.TempValue],
      varArgTempOpt: Option[ps.TempValue]
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
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

    val argTemps = worldTemps ++ selfTemps ++ fixedTemps ++ varArgTempOpt

    val discardable = !invokableProc.hasSideEffects(fixedTemps.length)

    signature.returnType match {
      case vt.ReturnType.Unreachable =>
        plan.steps += ps.Invoke(None, signature, entryPointTemp, argTemps, discardable=discardable)
        iv.UnreachableValue

      case vt.ReturnType.Reachable(vt.UnitType) =>
        plan.steps += ps.Invoke(None, signature, entryPointTemp, argTemps, discardable=discardable)

        if (signature.attributes.contains(ProcedureAttribute.NoReturn)) {
          iv.UnreachableValue
        }
        else {
          iv.UnitValue
        }

      case vt.ReturnType.Reachable(valueType) =>
        val resultTemp = ps.TempValue()
        plan.steps += ps.Invoke(Some(resultTemp), signature, entryPointTemp, argTemps, discardable=discardable)

        TempValueToIntermediate(valueType, resultTemp)
    }
  }

  def withArgumentList(
      invokableProc: iv.InvokableProc,
      argListValue: iv.IntermediateValue
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
    val signature = invokableProc.polySignature.upperBound

    val insufficientArgsMessage = ArityRuntimeErrorMessage.insufficientArgs(invokableProc)

    val improperListMessage = RuntimeErrorMessage(
      category=ErrorCategory.Type,
      text="Attempted application with improper list"
    )

    // Split our arguments in to fixed args and a var arg
    val destructuredArgs = DestructureList(
      listValue=argListValue,
      memberTypes=signature.mandatoryArgTypes,
      insufficientLengthMessage=insufficientArgsMessage,
      improperListMessageOpt=Some(improperListMessage)
    )

    val fixedArgTemps = destructuredArgs.memberTemps
    val varArgValue = destructuredArgs.listTailValue

    val varArgTempOpt = if (signature.restArgMemberTypeOpt.isDefined || (signature.optionalArgTypes.length > 0)) {
      val requiredVarArgType = vt.VariableArgsToListType(
        signature.optionalArgTypes,
        signature.restArgMemberTypeOpt
      )

      val typeCheckedVarArg = varArgValue.toTempValue(requiredVarArgType)

      Some(typeCheckedVarArg)
    }
    else {
      val tooManyArgsMessage = ArityRuntimeErrorMessage.tooManyArgs(invokableProc)

      // Make sure we're out of args by doing a check cast to an empty list
      varArgValue.toTempValue(vt.EmptyListType, Some(tooManyArgsMessage))
      None
    }

    PlanInvokeApply.withTempValues(invokableProc, fixedArgTemps, varArgTempOpt)
  }

  def withIntermediateValues(
      invokableProc: iv.InvokableProc,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
    val signature = invokableProc.polySignature.upperBound

    // Convert all the args
    val mandatoryTemps = args.zip(signature.mandatoryArgTypes) map { case ((contextLocated, arg), nativeType) =>
      plan.withContextLocation(contextLocated) {
        arg.toTempValue(nativeType)
      }
    }

    val varArgTempOpt = if (signature.restArgMemberTypeOpt.isDefined || (signature.optionalArgTypes.length > 0)) {
      val varArgs = args.drop(signature.mandatoryArgTypes.length)

      val optionalArgValues = varArgs.zip(signature.optionalArgTypes).map {
        case ((contextLocated, argValue), schemeType) =>
          plan.withContextLocation(contextLocated) {
            argValue.castToSchemeType(schemeType)
          }
      }

      val restArgs = varArgs.drop(signature.optionalArgTypes.length)

      val restArgValues = restArgs map { case (contextLocated, argValue) =>
        plan.withContextLocation(contextLocated) {
          argValue.castToSchemeType(signature.restArgMemberTypeOpt.get)
        }
      }

      val varArgValues = optionalArgValues ++ restArgValues
      Some(ValuesToList(varArgValues).toTempValue(vt.ListElementType))
    }
    else {
      None
    }

    PlanInvokeApply.withTempValues(invokableProc, mandatoryTemps, varArgTempOpt)
  }
}

