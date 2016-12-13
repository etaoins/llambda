package io.llambda.compiler.planner.conniver
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
  private def replaceTailStep(tailStep: ps.Step, retValueOpt: Option[ps.TempValue]): (ps.Step, Boolean) = tailStep match {
    case ps.Invoke(`retValueOpt`, signature, entryPoint, arguments, _, _)  =>
      // This is a tail call - replace the ps.Invoke with an equivalent ps.TailCall
      val replacedStep = ps.TailCall(signature, entryPoint, arguments)
        .assignLocationFrom(tailStep)

      // We can discard the return here
      (replacedStep, true)

    case ps.CondBranch(test, trueSteps, falseSteps, valuePhis @ List(ps.ValuePhi(condResult, trueValue, falseValue)))
        if retValueOpt.isDefined && retValueOpt.get == condResult =>
      // We can replace any tail calls that appear at the end of the condition branches
      val replacedTrueSteps = replaceBranchTailCalls(trueSteps.reverse, trueValue)
      val replacedFalseSteps = replaceBranchTailCalls(falseSteps.reverse, falseValue)

      val replacedStep = ps.CondBranch(test, replacedTrueSteps, replacedFalseSteps, valuePhis)
        .assignLocationFrom(tailStep)

      // We might still need the return if one of the branches wasn't converted to a tail call
      (replacedStep, false)

    case other =>
      (other, false)
  }

  private def replaceBranchTailCalls(reverseSteps: List[ps.Step], branchResult: ps.TempValue): List[ps.Step] =
    reverseSteps match {
      case tailStep :: reverseTail =>
        val (replacedStep, _) = replaceTailStep(tailStep, Some(branchResult))
        replaceTailCalls(reverseTail, List(replacedStep))

      case Nil =>
        Nil
    }

  private def replaceTailCalls(reverseSteps: List[ps.Step], acc: List[ps.Step]): List[ps.Step] =
    reverseSteps match {
      case (retStep @ ps.Return(retValueOpt)) :: tailStep :: reverseTail =>
        // Attempt to replace the step
        // This will only succeed for certain steps that replaceTailStep can handle and if those steps are producing
        // the expected return value
        val (replacedStep, discardReturn) = replaceTailStep(tailStep, retValueOpt)

        if (discardReturn) {
          replaceTailCalls(reverseTail, List(replacedStep))
        }
        else {
          replaceTailCalls(reverseTail, List(replacedStep, retStep))
        }

      case other :: reverseTail =>
        replaceTailCalls(reverseTail, other :: acc)

      case Nil =>
        acc
    }

  protected[conniver] def conniveSteps(steps: List[ps.Step]): List[ps.Step] =
    replaceTailCalls(steps.reverse, Nil)

  protected def conniveFunction(function: PlannedFunction): PlannedFunction = {
    function.copy(
      steps=conniveSteps(function.steps)
    )
  }
}
