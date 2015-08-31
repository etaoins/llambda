package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{ErrorCategory, RuntimeErrorMessage}
import llambda.compiler.planner.{intermediatevalue => iv}

object ArityRuntimeErrorMessage {
  private def arityError(
      errorType : String,
      invokableProc : iv.InvokableProc
  )(implicit plan : PlanWriter) : RuntimeErrorMessage = {
    val signature = invokableProc.polySignature.template
    val nativeSymbol = invokableProc.nativeSymbolOpt.getOrElse("procedure")
    val requiredArity = RequiredArityDescription.fromProcedureSignature(signature)

    val compactErrorTypeName = errorType.replace(" ", "")
    val compactArityName = requiredArity.replace(" ", "")

    RuntimeErrorMessage(
      category=ErrorCategory.Arity,
      name=s"${compactErrorTypeName}For${nativeSymbol}Requires${compactArityName}",
      text=s"Called ${nativeSymbol} with ${errorType}; requires ${requiredArity}."
    )
  }

  def insufficientArgs(invokableProc : iv.InvokableProc)(implicit plan : PlanWriter) : RuntimeErrorMessage = {
    arityError("insufficient arguments", invokableProc)
  }

  def tooManyArgs(invokableProc : iv.InvokableProc)(implicit plan : PlanWriter) : RuntimeErrorMessage = {
    arityError("too many arguments", invokableProc)
  }
}
