package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{ContextLocated, ReturnType}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

object PlanInvokeApply {
  def invokeWithTempValues(
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
      case ReturnType.SingleValue(vt.UnitType) =>
        plan.steps += ps.Invoke(None, signature, entryPointTemp, invokeArgs)
        SingleValue(iv.UnitValue)

      case otherType =>
        val resultTemp = ps.Temp(otherType.representationTypeOpt.get)
        plan.steps += ps.Invoke(Some(resultTemp), signature, entryPointTemp, invokeArgs)

        TempValueToResults(otherType, resultTemp)
    }
  }


  def apply(invokableProc : InvokableProcedure, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ResultValues = {
    val signature = invokableProc.signature

    // Convert all the operands
    val fixedTemps = operands.zip(signature.fixedArgs) map { case ((contextLocated, operand), nativeType) =>
      plan.withContextLocation(contextLocated) {
        operand.toTempValue(nativeType)
      }
    }

    val restTemps = signature.restArgOpt map { memberType =>
      val restOperands = operands.drop(signature.fixedArgs.length)

      val restArgs = restOperands map { case (contextLocated, restValue) =>
        plan.withContextLocation(contextLocated) {
          restValue.castToSchemeType(memberType)
        }
      }

      ValuesToProperList(restArgs).toTempValue(vt.ListElementType)
    }

    invokeWithTempValues(invokableProc, fixedTemps, restTemps)
  }
}

