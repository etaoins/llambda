package llambda.planner

import llambda.nfi
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}
import llambda.{valuetype => vt}
import llambda.{boxedtype => bt}

object PlanApplication {
  private def boxRestArgs(restArgs : List[iv.IntermediateValue])(implicit plan : PlanWriter) : ps.TempValue = {
    val restArgCount = restArgs.length

    if (restArgCount == 0) {
      // Avoid a cons allocation here so our plan optimizer knows we don't need GC
      val emptyListTemp = new ps.TempValue
      plan.steps += ps.StoreBoxedEmptyList(emptyListTemp)

      val listElemCast = new ps.TempValue
      plan.steps += ps.CastBoxedToTypeUnchecked(listElemCast, emptyListTemp, bt.BoxedListElement)

      listElemCast
    }
    else {
      val allocTemp = new ps.TempAllocation
      val restArgTemp = new ps.TempValue

      val argTemps = restArgs.map {
        _.toRequiredTempValue(vt.BoxedValue(bt.BoxedDatum))
      }

      plan.steps += ps.AllocateCons(allocTemp, restArgCount)
      plan.steps += ps.BuildProperList(restArgTemp, allocTemp, 0, argTemps)

      restArgTemp
    }
  }

  def apply(invokableProc : InvokableProcedure, operandValues : List[iv.IntermediateValue])(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    val entryPointTemp = invokableProc.planEntryPoint()
    val signature = invokableProc.signature

    // Ensure our arity is sane
    if (signature.hasRestArg) {
      if (operandValues.length < signature.fixedArgs.length) {
        throw new UnlocatedIncompatibleArityException(s"Called function with ${operandValues.length} arguments; requires at least ${signature.fixedArgs.length} arguments")
      }
    }
    else {
      if (signature.fixedArgs.length != operandValues.length) {
        throw new UnlocatedIncompatibleArityException(s"Called function with ${operandValues.length} arguments; requires exactly ${signature.fixedArgs.length} arguments")
      }
    }

    val closureTemps = if (signature.hasClosureArg) {
      invokableProc.planClosure() :: Nil
    }
    else {
      Nil
    }

    // Convert all the operands
    val fixedTemps = operandValues.zip(signature.fixedArgs) map { case (operandValue, nativeType) =>
      operandValue.toRequiredTempValue(nativeType)
    }

    val restTemps = if (signature.hasRestArg) {
      boxRestArgs(operandValues.drop(signature.fixedArgs.length)) :: Nil
    }
    else {
      Nil
    }

    val argTemps = closureTemps ++ fixedTemps ++ restTemps

    val resultTemp = signature.returnType.map { _ =>
      new ps.TempValue
    }

    plan.steps += ps.Invoke(resultTemp, signature, entryPointTemp, argTemps)

    resultTemp.map { tempValue =>
      TempValueToIntermediate(signature.returnType.get, tempValue)
    }
  }
}

