package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.RuntimeErrorMessage

object ArityRuntimeErrorMessage {
  def insufficientArgs(invokableProc : InvokableProcedure)(implicit plan : PlanWriter) : RuntimeErrorMessage = {
    val signature = invokableProc.polySignature.template
    val nativeSymbol = invokableProc.nativeSymbolOpt.getOrElse("procedure")
    val fixedArgCount = signature.fixedArgTypes.length

    if (signature.restArgMemberTypeOpt.isDefined) {
      RuntimeErrorMessage(
        name=s"insufficientArgsFor${nativeSymbol}RequiresAtLeast${fixedArgCount}",
        text=s"Called ${nativeSymbol} with insufficient arguments; requires at least ${fixedArgCount} arguments."
      )
    }
    else {
      RuntimeErrorMessage(
        name=s"insufficientArgsFor${nativeSymbol}RequiresExactly${fixedArgCount}",
        text=s"Called ${nativeSymbol} with insufficient arguments; requires exactly ${fixedArgCount} arguments."
      )
    }
  }

  def tooManyArgs(invokableProc : InvokableProcedure)(implicit plan : PlanWriter) : RuntimeErrorMessage = {
    val signature = invokableProc.polySignature.template
    val nativeSymbol = invokableProc.nativeSymbolOpt.getOrElse("procedure")
    val fixedArgCount = signature.fixedArgTypes.length

    RuntimeErrorMessage(
      name=s"tooManyArgsFor${nativeSymbol}Requires${fixedArgCount}",
      text=s"Called ${nativeSymbol} with too many arguments; requires exactly ${fixedArgCount} arguments."
    )
  }
}
