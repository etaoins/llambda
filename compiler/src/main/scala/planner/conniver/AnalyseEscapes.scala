package io.llambda.compiler.planner.conniver
import io.llambda

import llambda.compiler.planner._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.ProcedureAttribute

/** Performs escape analysis on planned functions
  *
  * This will convert garbage collected heap allocations to stack allocations where possible.
  */
object AnalyseEscapes extends FunctionConniver {
  private def convertToStack(
      reverseSteps: List[ps.Step],
      capturedValues: Set[ps.TempValue],
      acc: List[ps.Step]
  ): (List[ps.Step], Set[ps.TempValue]) = reverseSteps match {
    case (stackAllocable: ps.StackAllocableStep) :: reverseTail =>
      val newStep = if (!capturedValues.contains(stackAllocable.result)) {
        // Our output isn't captured; allocate on the stack
        stackAllocable.asStackAllocated
      }
      else {
        stackAllocable
      }

      convertToStack(reverseTail, capturedValues, newStep :: acc)

    case (cond: ps.CondBranch) :: reverseTail =>
      // We can use the same values in the true and false branch; it doesn't matter if values not used in that branch
      // appear in their captured values list
      val branchCapturedValues = cond.valuePhis.flatMap({ case ps.ValuePhi(result, trueValue, falseValue) =>
        if (capturedValues.contains(result)) {
          List(trueValue, falseValue)
        }
        else {
          List()
        }
      }).toSet

      val (mappedTrueSteps, trueCaptured) = convertToStack(cond.trueSteps.reverse, branchCapturedValues, Nil)
      val (mappedFalseSteps, falseCaptured) = convertToStack(cond.falseSteps.reverse, branchCapturedValues, Nil)

      val newCapturedValues = capturedValues ++ trueCaptured ++ falseCaptured
      val newCond = cond.copy(trueSteps=mappedTrueSteps, falseSteps=mappedFalseSteps).assignLocationFrom(cond)

      convertToStack(reverseTail, newCapturedValues, newCond :: acc)

    case (cast: ps.CastCellToTypeUnchecked) :: reverseTail =>
      // This captures only if the result is captured
      val newCapturedValues = if (capturedValues.contains(cast.result)) {
        capturedValues ++ cast.inputValues
      }
      else {
        capturedValues
      }

      convertToStack(reverseTail, newCapturedValues, cast :: acc)

    case (other @ (_: ps.UnboxValue | _: ps.TestCellType)) :: reverseTail =>
      // These do not capture
      convertToStack(reverseTail, capturedValues, other :: acc)

    // Note that we *cannot* convert tail calls - they take over our stack so they cannot accept stack allocated values
    case (invoke: ps.Invoke) :: reverseTail =>
      val newCapturedValues = if (invoke.signature.attributes.contains(ProcedureAttribute.NoCapture)) {
        capturedValues
      }
      else {
        capturedValues ++ invoke.inputValues
      }

      convertToStack(reverseTail, newCapturedValues, invoke :: acc)

    case other :: reverseTail =>
      // Assume all values are captured
      val newCapturedValues = capturedValues ++ other.inputValues
      convertToStack(reverseTail, newCapturedValues, other :: acc)

    case Nil =>
      (acc, capturedValues)
  }

  protected[conniver] def conniveSteps(steps: List[ps.Step]): List[ps.Step] =
    convertToStack(steps.reverse, Set(), Nil)._1

  protected[conniver] def conniveFunction(function: PlannedFunction): PlannedFunction = {
    function.copy(
      steps=conniveSteps(function.steps)
    )
  }
}
