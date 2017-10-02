package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{ErrorCategory, RuntimeErrorMessage}
import llambda.compiler.planner.{intermediatevalue => iv}

object ArityRuntimeErrorMessage {
  private def arityError(
      errorType: String,
      procedureValue: iv.ProcedureValue
  )(implicit plan: PlanWriter): RuntimeErrorMessage = {
    val procedureType = procedureValue.polyProcedureType.upperBound

    val nativeSymbol = procedureValue.nativeSymbolOpt.getOrElse("procedure")
    val requiredArity = RequiredArityDescription.fromProcedureType(procedureType)

    RuntimeErrorMessage(
      category=ErrorCategory.Arity,
      text=s"Called ${nativeSymbol} with ${errorType}; requires ${requiredArity}."
    )
  }

  def insufficientArgs(procedureValue: iv.ProcedureValue)(implicit plan: PlanWriter): RuntimeErrorMessage = {
    arityError("insufficient arguments", procedureValue)
  }

  def tooManyArgs(procedureValue: iv.ProcedureValue)(implicit plan: PlanWriter): RuntimeErrorMessage = {
    arityError("too many arguments", procedureValue)
  }
}
