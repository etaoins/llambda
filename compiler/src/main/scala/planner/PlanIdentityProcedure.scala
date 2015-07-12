package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

import llambda.compiler.InternalCompilerErrorException

private[planner] object PlanIdentityProcedure {
  /** Plans a procedure that will return its only argument
    *
    * @param  signature  Target signature for the procedure. This must be an adapted procedure signature.
    */
  def apply(signature : ProcedureSignature)(implicit parentPlan : PlanWriter) : PlannedFunction = {
    implicit val plan = parentPlan.forkPlan()

    if (!signature.hasWorldArg || !signature.hasSelfArg) {
      throw new InternalCompilerErrorException("Attempted to plan identity procedure with incompatible signature")
    }

    val (fixedArgTemps, fixedArgValues) = signature.fixedArgTypes.map({ fixedArgType =>
      val fixedArgTemp = ps.Temp(fixedArgType)
      val fixedArgValue = TempValueToIntermediate(fixedArgType, fixedArgTemp)(plan.config)

      (fixedArgTemp, fixedArgValue)
    }).unzip

    val (restArgTempIter, restArgValueIter) = signature.restArgMemberTypeOpt.map({ restArgMemberType =>
      val restArgTemp = ps.CellTemp(ct.ListElementCell)
      val restArgType = vt.UniformProperListType(restArgMemberType)
      val restArgValue = new iv.CellValue(restArgType, BoxedValue(ct.ListElementCell, restArgTemp))

      (restArgTemp, restArgValue)
    }).unzip

    val valuesTail = if (restArgValueIter.isEmpty) iv.EmptyListValue else restArgValueIter.head
    val valueList = ValuesToList(fixedArgValues, valuesTail, capturable=false)(plan)

    val returnTempOpt = MultipleValues(valueList).toReturnTempValue(signature.returnType)(plan)
    plan.steps += ps.Return(returnTempOpt)

    val inNamedArguments = List(
      ("world"   -> ps.WorldPtrValue),
      ("closure" -> ps.CellTemp(ct.ProcedureCell))
    ) ++ fixedArgTemps.zipWithIndex.map { case (fixedArgTemp, index) =>
      s"fixedArg${index}" -> fixedArgTemp
    } ++ restArgTempIter.toList.map { case restArgTemp =>
      "restArg" -> restArgTemp
    }

    PlannedFunction(
      signature=signature,
      namedArguments=inNamedArguments,
      steps=plan.steps.toList,
      debugContextOpt=None
    )
  }
}
