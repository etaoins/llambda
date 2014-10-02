package io.llambda.compiler.conniver
import io.llambda

import llambda.compiler.planner._
import llambda.compiler.planner.{step => ps}

/** Replaces any Invoke step followed immediately by a matching Return with a TailCall step
  *
  * The TailCall step is essentially a fused Invoke + Return that is handled specially by codegen. In particular, it
  * will remove the current function's shadow stack entry and pass a tail call hint to LLVM. This allows many recursive
  * procedures to use bounded space as required by Scheme.
  */
object FindTailCalls extends FunctionConniver {
  private def invokeToTailCall(invokeStep : ps.Invoke) : ps.TailCall =
    ps.TailCall(invokeStep.signature, invokeStep.entryPoint, invokeStep.arguments.map(_.tempValue))
      .assignLocationFrom(invokeStep)
        
      
  private def replaceBranchTailCalls(reverseSteps : List[ps.Step], branchResult : ps.TempValue) : List[ps.Step] =
    reverseSteps match {
      case (invokeStep @ ps.Invoke(Some(`branchResult`), _, _, _)) :: reverseTail =>
        // We have an invoke at the tail of the branch that would normally be phi'ed and returned
        // Convert it to a tail call
        val replacedStep = invokeToTailCall(invokeStep)
        replaceTailCalls(reverseTail, List(replacedStep))

      case nonTailResult =>
        // Not a tail call but check higher up in the branch for tail calls
        replaceTailCalls(nonTailResult, Nil)
    }

  private def replaceTailCalls(reverseSteps : List[ps.Step], acc : List[ps.Step]) : List[ps.Step] =
    reverseSteps match {
      case ps.Return(retValue) ::
           (invokeStep @ ps.Invoke(invokeResult, _, _, _)) ::
           reverseTail
      if retValue == invokeResult =>
        val replacedStep = invokeToTailCall(invokeStep)
        // We can discard all further steps here
        replaceTailCalls(reverseTail, replacedStep :: Nil)

      case (retStep @ ps.Return(Some(retValue))) ::
           (condStep @ ps.CondBranch(condResult, test, trueSteps, trueValue, falseSteps, falseValue)) ::
           reverseTail
      if retValue == condResult =>
        // This is tricky - we're tail calling using the result of a condition
        val replacedTrueSteps = replaceBranchTailCalls(trueSteps.reverse, trueValue)
        val replacedFalseSteps = replaceBranchTailCalls(falseSteps.reverse, falseValue)

        val replacedStep = ps.CondBranch(condResult, test, replacedTrueSteps, trueValue, replacedFalseSteps, falseValue)
        replaceTailCalls(reverseTail, replacedStep :: retStep :: acc)

      case other :: reverseTail =>
        replaceTailCalls(reverseTail, other :: acc)

      case Nil =>
        acc
    }

  protected[conniver] def conniveSteps(steps : List[ps.Step]) : List[ps.Step] =
      replaceTailCalls(steps.reverse, Nil)

  protected def conniveFunction(function : PlannedFunction) : PlannedFunction = {
    function.copy(
      steps=conniveSteps(function.steps)
    )
  }
}
