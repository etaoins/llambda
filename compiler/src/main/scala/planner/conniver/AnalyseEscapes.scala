package io.llambda.compiler.planner.conniver
import io.llambda

import llambda.compiler.planner._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.ProcedureAttribute

/** Performs escape analysis on planned functions
  *
  * This will convert garbage collected heap allocations to stack allocations where possible. Escape is defined in the
  * following ways:
  *
  * 1) The value (or one of its subvalues) is returned
  * 2) The value is referenced by another value that is captured
  * 3) The value is passed as an argument to another function that may capture it
  * 4) The value is thrown as part of a signalled exception
  * 5) The value is referenced by a heap allocated value and the reference is not reset before the function returns.
  *    This is because the garbage collector will attempt the trace the object graph and must not following pointers
  *    to stale heap allocated space.
  */
object AnalyseEscapes extends FunctionConniver {
  private def convertToStack(
      reverseSteps: List[ps.Step],
      capturedValues: Set[ps.TempValue],
      acc: List[ps.Step]
  ): (List[ps.Step], Set[ps.TempValue]) = reverseSteps match {
    case (stackAllocable: ps.StackAllocableStep) :: reverseTail =>
      val (newStep, newCapturedValues) = if (!capturedValues.contains(stackAllocable.result)) {
        // Our output isn't captured; allocate on the stack
        (stackAllocable.asStackAllocated, capturedValues)
      }
      else {
        // Because our result is captured our inputs are also captured
        (stackAllocable, capturedValues ++ stackAllocable.inputValues)
      }

      convertToStack(reverseTail, newCapturedValues, newStep :: acc)

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

    case (transferring @ (_: ps.CastCellToTypeUnchecked | _: ps.LoadPairValue)) :: reverseTail =>
      // This captures only if one of its outputs is captured
      val newCapturedValues = if ((capturedValues & transferring.outputValues).isEmpty) {
        capturedValues
      }
      else {
        capturedValues ++ transferring.inputValues
      }

      convertToStack(reverseTail, newCapturedValues, transferring :: acc)

    case (nonCapturing @ (_: ps.UnboxValue | _: ps.TestCellType | _: ps.CalcProperListLength)) :: reverseTail =>
      // These do not capture
      convertToStack(reverseTail, capturedValues, nonCapturing :: acc)

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
