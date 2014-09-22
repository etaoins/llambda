package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.RuntimeErrorMessage

object ArityRuntimeErrorMessage {
  def insufficientArgs(invokableProc : InvokableProcedure)(implicit plan : PlanWriter) : RuntimeErrorMessage = {
    val signature = invokableProc.signature
    val nativeSymbol = invokableProc.nativeSymbolOpt.getOrElse("procedure")

    if (signature.restArgMemberTypeOpt.isDefined) {
      RuntimeErrorMessage(
        name=s"insufficientArgsFor${nativeSymbol}",
        text=s"Called ${nativeSymbol} with insufficient arguments; requires at least ${signature.fixedArgTypes.length} arguments."
      )
    }
    else {
      RuntimeErrorMessage(
        name=s"insufficientArgsFor${nativeSymbol}",
        text=s"Called ${nativeSymbol} with insufficient arguments; requires exactly ${signature.fixedArgTypes.length} arguments."
      )
    }
  }

  def tooManyArgs(invokableProc : InvokableProcedure)(implicit plan : PlanWriter) : RuntimeErrorMessage = {
    val signature = invokableProc.signature
    val nativeSymbol = invokableProc.nativeSymbolOpt.getOrElse("procedure")
        
    RuntimeErrorMessage(
      name=s"tooManyArgsFor${nativeSymbol}",
      text=s"Called ${nativeSymbol} with too many arguments; requires exactly ${signature.fixedArgTypes.length} arguments."
    )
  }
}
