package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{ContextLocated, RuntimeErrorMessage}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

import llambda.compiler.valuetype.Implicits._

object PlanInvokeApply {
  def withTempValues(
      invokableProc : InvokableProcedure,
      fixedTemps : Seq[ps.TempValue],
      restTemps : Option[ps.TempValue],
      selfTempOverride : Option[ps.TempValue] = None
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ResultValues = {
    val entryPointTemp = invokableProc.planEntryPoint()
    val signature = invokableProc.signature

    val worldTemps = if (signature.hasWorldArg) {
      List(worldPtr)
    }
    else {
      Nil
    }

    val selfTemps = if (signature.hasSelfArg) {
      List(selfTempOverride.getOrElse {
        invokableProc.planSelf()
      })
    }
    else {
      Nil
    }
    
    val argTemps = worldTemps ++ selfTemps ++ fixedTemps ++ restTemps
    val invokeArgs = argTemps.toList.map(ps.InvokeArgument(_))

    signature.returnType match {
      case vt.ReturnType.SingleValue(vt.UnitType) =>
        plan.steps += ps.Invoke(None, signature, entryPointTemp, invokeArgs)
        SingleValue(iv.UnitValue)

      case otherType =>
        val resultTemp = ps.Temp(otherType.representationTypeOpt.get)
        plan.steps += ps.Invoke(Some(resultTemp), signature, entryPointTemp, invokeArgs)

        TempValueToResults(otherType, resultTemp)
    }
  }

  def withArgumentList(
      invokableProc : InvokableProcedure,
      argListValue : iv.IntermediateValue,
      selfTempOverride : Option[ps.TempValue] = None
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ResultValues = {
    val signature = invokableProc.signature
    val argListType = vt.UniformProperListType(vt.AnySchemeType)
    
    val insufficientArgsMessage = ArityRuntimeErrorMessage.insufficientArgs(invokableProc)

    // Split our arguments in to fixed args and a rest arg
    val destructuredArgs = DestructureList(argListValue, signature.fixedArgTypes, insufficientArgsMessage)

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

    PlanInvokeApply.withTempValues(invokableProc, fixedArgTemps, restArgTemps, selfTempOverride) 
  }


  def withIntermediateValues(
      invokableProc : InvokableProcedure,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ResultValues = {
    val signature = invokableProc.signature

    // Convert all the operands
    val fixedTemps = operands.zip(signature.fixedArgTypes) map { case ((contextLocated, operand), nativeType) =>
      plan.withContextLocation(contextLocated) {
        operand.toTempValue(nativeType)
      }
    }

    val restTemps = signature.restArgMemberTypeOpt map { memberType =>
      val restOperands = operands.drop(signature.fixedArgTypes.length)

      val restArgs = restOperands map { case (contextLocated, restValue) =>
        plan.withContextLocation(contextLocated) {
          restValue.castToSchemeType(memberType)
        }
      }

      ValuesToList(restArgs, capturable=false).toTempValue(vt.ListElementType)
    }

    PlanInvokeApply.withTempValues(invokableProc, fixedTemps, restTemps)
  }
}

