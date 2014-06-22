package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.ContextLocated
import llambda.compiler.IncompatibleArityException
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

object PlanApplication {
  private def boxRestArgs(restArgs : List[iv.IntermediateValue])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val restArgCount = restArgs.length

    if (restArgCount == 0) {
      // Avoid a cell allocation here so our plan optimizer knows we don't need GC

      // This is unmanaged because the empty list is a constant value
      val emptyListTemp = ps.CellTemp(ct.EmptyListCell, knownConstant=true)
      plan.steps += ps.CreateEmptyListCell(emptyListTemp)

      // We know this is constant because the empty list is constant
      val listElemCast = ps.CellTemp(ct.ListElementCell, knownConstant=true)
      plan.steps += ps.CastCellToTypeUnchecked(listElemCast, emptyListTemp, ct.ListElementCell)

      listElemCast
    }
    else {
      val restArgTemp = ps.CellTemp(ct.ListElementCell)

      val argTemps = restArgs.map {
        _.toTempValue(vt.AnySchemeType)
      }

      plan.steps += ps.BuildProperList(restArgTemp, argTemps)

      restArgTemp
    }
  }

  def apply(invokableProc : InvokableProcedure, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = {
    val entryPointTemp = invokableProc.planEntryPoint()
    val signature = invokableProc.signature

    // Ensure our arity is sane
    if (signature.hasRestArg) {
      if (operands.length < signature.fixedArgs.length) {
        throw new IncompatibleArityException(
          located=plan.activeContextLocated,
          message=s"Called procedure with ${operands.length} arguments; requires at least ${signature.fixedArgs.length} arguments"
        )
      }
    }
    else {
      if (signature.fixedArgs.length != operands.length) {
        throw new IncompatibleArityException(
          located=plan.activeContextLocated,
          message=s"Called procedure with ${operands.length} arguments; requires exactly ${signature.fixedArgs.length} arguments"
        )
      }
    }

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

    val restTemps = if (signature.hasRestArg) {
      boxRestArgs(operands.map(_._2).drop(signature.fixedArgs.length)) :: Nil
    }
    else {
      Nil
    }

    val argTemps = worldTemps ++ selfTemps ++ fixedTemps ++ restTemps

    val resultTemp = signature.returnType.map { returnType =>
      new ps.TempValue(returnType.isGcManaged)
    }

    val invokeArgs = argTemps.toList.map(ps.InvokeArgument(_))
    plan.steps += ps.Invoke(resultTemp, signature, entryPointTemp, invokeArgs)

    resultTemp.map { tempValue =>
      TempValueToIntermediate(signature.returnType.get, tempValue)
    }
  }
}

