package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{ErrorCategory, RuntimeErrorMessage}
import llambda.compiler.planner.{intermediatevalue => iv}

object ArityRuntimeErrorMessage {
  private def arityError(
      errorType: String,
      applicableValue: iv.ApplicableValue
  )(implicit plan: PlanWriter): RuntimeErrorMessage = {
    val signature = applicableValue.polySignature.template
    val nativeSymbol = applicableValue.nativeSymbolOpt.getOrElse("procedure")
    val requiredArity = RequiredArityDescription.fromProcedureSignature(signature)

    RuntimeErrorMessage(
      category=ErrorCategory.Arity,
      text=s"Called ${nativeSymbol} with ${errorType}; requires ${requiredArity}."
    )
  }

  def insufficientArgs(applicableValue: iv.ApplicableValue)(implicit plan: PlanWriter): RuntimeErrorMessage = {
    arityError("insufficient arguments", applicableValue)
  }

  def tooManyArgs(applicableValue: iv.ApplicableValue)(implicit plan: PlanWriter): RuntimeErrorMessage = {
    arityError("too many arguments", applicableValue)
  }
}
