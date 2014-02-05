package io.llambda.compiler.planner
import io.llambda

import io.llambda.compiler.planner.{step => ps}

/** Inserts AllocateCell steps to satisfy the allocations for cell consumers 
 *
 * This should be the very final pass after planning and conniving
 */ 
object PlanCellAllocations {
  private case class MergeAllResult(
    requiredSize : Int,
    steps : List[ps.Step]
  )

  private def mergeAllAllocationsTo(steps : List[ps.Step], consumedCells : Int) : MergeAllResult = steps match {
    case (consumer : ps.CellConsumer) :: tailSteps =>
      // Track the number of cells this consumer requires
      val tailResult = mergeAllAllocationsTo(tailSteps, consumedCells + consumer.allocSize)
      
      tailResult.copy(
        steps=consumer :: tailResult.steps
      )

    case (nestingStep : ps.NestingStep) :: tailSteps =>
      // It would only be possible to merge allocations across branches if they
      // both allocate the exact same number of cells. Otherwise one branch
      // would have extra uninitialized cells which would confuse our GC.

      // Another option would be to allocate the minimum number of allocations
      // and have the larger branch allocate the difference

      // Both of these seem to have high complexity for the benefit they'd create

      // Abort and switch back to searching both branches and the remaining steps
      val newStep = nestingStep.mapInnerBranches { (steps, _) =>
        findNextConsumer(steps)
      }

      val steps = newStep :: findNextConsumer(tailSteps)

      MergeAllResult(
        requiredSize=consumedCells,
        steps=steps
      )

    case (barrier : ps.GcBarrier) :: tailSteps =>
      // We can't keep uninitialized cells across a GC barrier
      // Abort and switch back to searching
      val steps = findNextConsumer(barrier :: tailSteps)
      
      MergeAllResult(
        requiredSize=consumedCells,
        steps=steps
      )

    case other :: tailSteps =>
      // Pass this step through unmodified
      val tailResult = mergeAllAllocationsTo(tailSteps, consumedCells)
      
      tailResult.copy(
        steps=other :: tailResult.steps
      )

    case Nil =>
      MergeAllResult(
        requiredSize=consumedCells,
        steps=Nil
      )
  }

  private def findNextConsumer(steps : List[ps.Step]) : List[ps.Step] = steps match {
    case (consumer : ps.CellConsumer) :: _ =>
      // Found the next consumer - count the cells we need until the next GC step
      val mergeResult = mergeAllAllocationsTo(steps, 0)
      ps.AllocateCells(mergeResult.requiredSize) :: mergeResult.steps

    case (nestingStep : ps.NestingStep) :: tailSteps =>
      val newStep = nestingStep.mapInnerBranches { (steps, _) =>
        findNextConsumer(steps)
      }

      newStep :: findNextConsumer(tailSteps)

    case other :: tailSteps =>
      other :: findNextConsumer(tailSteps)

    case Nil =>
      Nil
  }

  def apply(function : PlannedFunction) : PlannedFunction = {
    function.copy(
      steps=findNextConsumer(function.steps)
    )
  }
}
