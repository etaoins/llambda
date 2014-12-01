package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

/** Inserts AllocateCell steps to satisfy the allocations for cell consumers 
  *
  * This should be the very final pass after planning and conniving
  */ 
object PlanCellAllocations {
  /** Prepends an allocation step to the front of the accumulator step 
    *
    * This takes care to allow a DisposeValues step to happen before the AllocateCells to reduce GC pressure
    */
  private def prependAllocStep(requiredCells : Int, acc : List[ps.Step]) = acc match {
    case _ if requiredCells == 0 =>
      // We don't need this step at all
      acc

    case (disposeStep : ps.DisposeValues) :: accTail =>
      // Dispose values before allocating so we don't need to root them across the allocation
      disposeStep :: ps.AllocateCells(requiredCells) :: accTail

    case otherAcc =>
      ps.AllocateCells(requiredCells) :: otherAcc
  }

  /** Returns true if a step can either consume or allocate cells
    *
    * This is used for ignoring branches that don't use GC when aggregating cell allocations
    */
  private def stepConsumesOrAllocates(step : ps.Step) : Boolean = step match {
    case consumer : ps.CellConsumer => 
      true
    case nestingStep : ps.NestingStep =>
      nestingStep.innerBranches.exists { case (steps, _) =>
        steps.exists(stepConsumesOrAllocates)
      }
    case otherStep => 
      otherStep.canAllocate
  }

  private def placeCellAllocations(reverseSteps : List[ps.Step], requiredCells : Int, acc : List[ps.Step]) : List[ps.Step] = reverseSteps match {
    case (consumer : ps.CellConsumer) :: reverseTail =>
      // Increment the cell count and keep processing
      val newAcc = consumer :: acc
      placeCellAllocations(reverseTail, requiredCells + 1, newAcc)

    case barrierStep :: reverseTail if barrierStep.canAllocate =>
      // This is a GC barrier - allocations can't cross this
      // Make our allocation step and then keep processing after resetting our required cell count accumulator
      val newAcc = barrierStep :: prependAllocStep(requiredCells, acc)
      placeCellAllocations(reverseTail, 0, newAcc) 

    case (nestingStep : ps.NestingStep) :: reverseTail if stepConsumesOrAllocates(nestingStep) =>
      // Recurse down each of the step's branches
      val newNestingStep = nestingStep.mapInnerBranches { (branchSteps, resultTemp) =>
        (placeCellAllocations(branchSteps.reverse, 0, Nil), resultTemp)
      }

      // Treat this as a GC barrier for now
      val newAcc = newNestingStep :: prependAllocStep(requiredCells, acc)
      placeCellAllocations(reverseTail, 0, newAcc) 

    case otherStep :: reverseTail =>
     // This step is neither a GC barrier nor consume cells
     // Just tail recurse
     val newAcc = otherStep :: acc
     placeCellAllocations(reverseTail, requiredCells, newAcc) 

    case Nil =>
      // We've reached the top of the branch - allocate any cells we need and terminate
      prependAllocStep(requiredCells, acc)
  }

  def apply(function : PlannedFunction) : PlannedFunction =
    if (function.signature.hasWorldArg) {
      function.copy(
        steps=placeCellAllocations(function.steps.reverse, 0, Nil)
      )
    }
    else {
      // No world pointer, no allocations
      function
    }
}
