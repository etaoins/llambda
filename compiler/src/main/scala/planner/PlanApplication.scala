package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.SourceLocated
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

object PlanApplication {
  private def boxRestArgs(restArgs : List[iv.IntermediateValue])(implicit plan : PlanWriter) : ps.TempValue = {
    val restArgCount = restArgs.length

    if (restArgCount == 0) {
      // Avoid a cell allocation here so our plan optimizer knows we don't need GC

      // This is unmanaged because the empty list is a constant value
      val emptyListTemp = ps.GcUnmanagedValue()
      plan.steps += ps.StoreEmptyListCell(emptyListTemp)

      val listElemCast = ps.GcUnmanagedValue()
      plan.steps += ps.CastCellToTypeUnchecked(listElemCast, emptyListTemp, ct.ListElementCell)

      listElemCast
    }
    else {
      val allocTemp = new ps.TempAllocation
      val restArgTemp = ps.GcManagedValue()

      val argTemps = restArgs.map {
        _.toTempValue(vt.IntrinsicCellType(ct.DatumCell))
      }

      plan.steps += ps.AllocateCells(allocTemp, restArgCount)
      plan.steps += ps.BuildProperList(restArgTemp, allocTemp, 0, argTemps)

      restArgTemp
    }
  }

  def apply(invokableProc : InvokableProcedure, operands : List[(SourceLocated, iv.IntermediateValue)])(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    val entryPointTemp = invokableProc.planEntryPoint()
    val signature = invokableProc.signature

    // Ensure our arity is sane
    if (signature.hasRestArg) {
      if (operands.length < signature.fixedArgs.length) {
        throw new UnlocatedIncompatibleArityException(s"Called procedure with ${operands.length} arguments; requires at least ${signature.fixedArgs.length} arguments")
      }
    }
    else {
      if (signature.fixedArgs.length != operands.length) {
        throw new UnlocatedIncompatibleArityException(s"Called procedure with ${operands.length} arguments; requires exactly ${signature.fixedArgs.length} arguments")
      }
    }

    val selfTemps = if (signature.hasSelfArg) {
      invokableProc.planSelf() :: Nil
    }
    else {
      Nil
    }

    // Convert all the operands
    val fixedTemps = operands.zip(signature.fixedArgs) map { case ((sourceLocated, operand), nativeType) =>
      LocateExceptionsWith(sourceLocated) {
        operand.toTempValue(nativeType)
      }
    }

    val restTemps = if (signature.hasRestArg) {
      boxRestArgs(operands.map(_._2).drop(signature.fixedArgs.length)) :: Nil
    }
    else {
      Nil
    }

    val argTemps = selfTemps ++ fixedTemps ++ restTemps

    val resultTemp = signature.returnType.map { returnType =>
      new ps.TempValue(returnType.isGcManaged)
    }

    plan.steps += ps.Invoke(resultTemp, signature, entryPointTemp, argTemps)

    resultTemp.map { tempValue =>
      TempValueToIntermediate(signature.returnType.get, tempValue)
    }
  }
}

