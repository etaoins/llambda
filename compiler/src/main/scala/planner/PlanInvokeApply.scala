package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.ContextLocated
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

object PlanInvokeApply {
  def apply(invokableProc : InvokableProcedure, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = {
    val entryPointTemp = invokableProc.planEntryPoint()
    val signature = invokableProc.signature

    val worldTemps = if (signature.hasWorldArg) {
      worldPtr :: Nil
    }
    else {
      Nil
    }

    val selfTemps = if (signature.hasSelfArg) {
      invokableProc.planSelf() :: Nil
    }
    else {
      Nil
    }

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

    val argTemps = worldTemps ++ selfTemps ++ fixedTemps ++ restTemps

    val resultTemp = signature.returnType.map { returnType =>
      new ps.TempValue(returnType.isGcManaged)
    }

    val invokeArgs = argTemps.toList.map(ps.InvokeArgument(_))
    plan.steps += ps.Invoke(resultTemp, signature, entryPointTemp, invokeArgs)

    resultTemp.map { tempValue =>
      TempValueToIntermediate(signature.returnType.get, tempValue)(plan.config)
    }
  }
}

