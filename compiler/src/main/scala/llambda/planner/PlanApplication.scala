package llambda.planner

import llambda.nfi
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}
import llambda.{boxedtype => bt}
import llambda.NotImplementedException

object PlanApplication {
  private def boxRestArgs(restArgs : List[iv.IntermediateValue])(implicit planSteps : StepBuffer) : ps.TempValue = {
    val restArgCount = restArgs.length

    if (restArgCount == 0) {
      // Avoid a cons allocation here so our plan optimizer knows we don't need GC
      val emptyListTemp = new ps.TempValue
      planSteps += ps.StoreBoxedEmptyList(emptyListTemp)

      val listElemCast = new ps.TempValue
      planSteps += ps.CastBoxedToTypeUnchecked(listElemCast, emptyListTemp, bt.BoxedListElement)

      listElemCast
    }
    else {
      val allocTemp = new ps.TempAllocation
      val restArgTemp = new ps.TempValue

      val argTemps = restArgs.map {
        _.toRequiredTempValue(nfi.BoxedValue(bt.BoxedDatum))
      }

      planSteps += ps.AllocateCons(allocTemp, restArgCount)
      planSteps += ps.BuildProperList(restArgTemp, allocTemp, 0, argTemps)

      restArgTemp
    }
  }

  def apply(invokableProc : InvokableProcedure, operandValues : List[iv.IntermediateValue])(implicit planSteps : StepBuffer) : Option[iv.IntermediateValue] = {
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

    val argTemps = fixedTemps ++ restTemps

    val resultTemp = signature.returnType.map { _ =>
      new ps.TempValue
    }

    planSteps += ps.Invoke(resultTemp, signature, entryPointTemp, argTemps)

    resultTemp.map { tempValue =>
      NativeToIntermediateValue(signature.returnType.get, tempValue)
    }
  }
}

