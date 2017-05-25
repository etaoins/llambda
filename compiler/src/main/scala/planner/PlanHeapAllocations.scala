package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

/** Inserts AllocateHeapCells steps to satisfy the allocations for cell consumers
  *
  * This should be the very final pass after planning and conniving
  */
object PlanHeapAllocations {
  /** Prepends an allocation step to the front of the accumulator step */
  private def prependAllocStep(requiredCells: Int, acc: List[ps.Step]) = acc match {
    case _ if requiredCells == 0 =>
      // We don't need this step at all
      acc

    case otherAcc =>
      ps.AllocateHeapCells(requiredCells) :: otherAcc
  }

  /** Returns true if a step can either consume cells or throw an exception
    *
    * This is used for ignoring branches that don't use GC when aggregating cell allocations
    */
  private def stepConsumesOrThrows(step: ps.Step): Boolean = step match {
    case condBranch: ps.CondBranch =>
      condBranch.innerBranches.exists { case (steps, _) =>
        steps.exists(stepConsumesOrThrows)
      }

    case otherStep =>
      (otherStep.requiredHeapCells > 0) || otherStep.canThrow
  }

  private def placeHeapAllocations(reverseSteps: List[ps.Step], requiredCells: Int, acc: List[ps.Step]): List[ps.Step] = reverseSteps match {
    case barrierStep :: reverseTail if barrierStep.canThrow =>
      // This step can throw an exception. This means we cannot have a partially used allocation at this point.
      val newAcc = barrierStep :: prependAllocStep(requiredCells, acc)
      placeHeapAllocations(reverseTail, 0, newAcc)

    case (condBranch: ps.CondBranch) :: reverseTail if stepConsumesOrThrows(condBranch) =>
      // Recurse down each of the step's branches
      val newCondBranch = condBranch.mapInnerBranches { (branchSteps, resultTemp) =>
        (placeHeapAllocations(branchSteps.reverse, 0, Nil), resultTemp)
      }

      // Treat this as a GC barrier for now
      val newAcc = newCondBranch :: prependAllocStep(requiredCells, acc)
      placeHeapAllocations(reverseTail, 0, newAcc)

    case otherStep :: reverseTail =>
      val newAcc = otherStep :: acc
      placeHeapAllocations(reverseTail, requiredCells + otherStep.requiredHeapCells, newAcc)

    case Nil =>
      // We've reached the top of the branch - allocate any cells we need and terminate
      prependAllocStep(requiredCells, acc)
  }

  def apply(function: PlannedFunction): PlannedFunction =
    if (function.signature.hasWorldArg) {
      function.copy(
        steps=placeHeapAllocations(function.steps.reverse, 0, Nil)
      )
    }
    else {
      // No world pointer, no allocations
      function
    }
}
