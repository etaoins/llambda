package io.llambda.compiler.planner
import io.llambda

import io.llambda.compiler.planner.{step => ps}

/** Inserts AllocateCell steps to satisfy the allocations for cell consumers 
 *
 * This should be the very final pass after planning and conniving
 */ 
object PlanCellAllocations {
  // Only returns an allocation step if we require more than zero cells
  // Allocating zero cells is actually handled properly by codegen but it's nice to omit them in case a human is
  // examining raw planner output
  private def allocCellSteps(requiredCells : Int)(implicit worldPtr : ps.WorldPtrValue) = if (requiredCells > 0) {
    List(ps.AllocateCells(worldPtr, requiredCells))
  }
  else {
    Nil
  }

  /** Returns true if a step can either consume or allocate cells
    *
    * This is used for ignoring branches that don't use GC when aggregating cell allocations
    */
  private def stepConsumesOrAllocates(step : ps.Step) : Boolean = step match {
    case consumer : ps.CellConsumer => 
      true
    case condStep : ps.CondBranch =>
      condStep.innerBranches.exists { case (steps, _) =>
        steps.exists(stepConsumesOrAllocates)
      }
    case otherStep => 
      otherStep.canAllocate
  }

  private def placeCellAllocations(reverseSteps : List[ps.Step], requiredCells : Int, acc : List[ps.Step])(implicit worldPtr : ps.WorldPtrValue) : List[ps.Step] = reverseSteps match {
    case (consumer : ps.CellConsumer) :: reverseTail =>
      // Add the consumer's cell count to our required cells and keep processing
      val newAcc = consumer :: acc
      placeCellAllocations(reverseTail, requiredCells + consumer.allocSize, newAcc)

    case allocatingStep :: reverseTail if allocatingStep.canAllocate =>
      // This is a GC barrier - allocations can't cross this
      // Make our allocation step and then keep processing after resetting our required cell count accumulator
      val newAcc = allocatingStep :: (allocCellSteps(requiredCells) ++ acc)
      placeCellAllocations(reverseTail, 0, newAcc) 

    case (condStep : ps.CondBranch) :: reverseTail if stepConsumesOrAllocates(condStep) =>
      // Recurse down each of the step's branches
      val newCondStep = condStep.mapInnerBranches { (branchSteps, resultTemp) =>
        (placeCellAllocations(branchSteps.reverse, 0, Nil), resultTemp)
      }

      // Treat this as a GC barrier for now
      val newAcc = newCondStep :: (allocCellSteps(requiredCells) ++  acc)
      placeCellAllocations(reverseTail, 0, newAcc) 

    case otherStep :: reverseTail =>
     // This step is neither a GC barrier nor consume cells
     // Just tail recurse
     val newAcc = otherStep :: acc
     placeCellAllocations(reverseTail, requiredCells, newAcc) 

    case Nil =>
      // We've reached the top of the branch - allocate any cells we need and terminate
      allocCellSteps(requiredCells) ++ acc
  }

  def apply(function : PlannedFunction) : PlannedFunction = function.worldPtrOption match {
    case Some(worldPtr) =>
      function.copy(
        steps=placeCellAllocations(function.steps.reverse, 0, Nil)(worldPtr)
      )
    case None =>
      // No world pointer, no allocations
      function
  }
}
