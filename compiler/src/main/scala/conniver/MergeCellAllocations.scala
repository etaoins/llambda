package io.llambda.compiler.conniver
import io.llambda

import io.llambda.compiler.planner._
import io.llambda.compiler.planner.{step => ps}

/** Merges multiple AllocateCells steps together until a GC barrier or branch
  * is reached.
  */
object MergeCellAllocations extends FunctionConniver {
  private case class MergeAllResult(
    requiredSize : Int,
    steps : List[ps.Step]
  )

  private def mergeAllAllocationsTo(steps : List[ps.Step], allocation : ps.TempAllocation, allocOffset : Int) : MergeAllResult = steps match {
    case (consumer : ps.CellConsumer) :: tailSteps =>
      // Merge this consumer in
      val tailResult = mergeAllAllocationsTo(tailSteps, allocation, allocOffset + consumer.allocSize)
      
      tailResult.copy(
        steps=consumer.withNewAllocation(allocation, allocOffset) :: tailResult.steps
      )

    case (_ : ps.AllocateCells) :: tailSteps =>
      // Kill this step completely; it's being merged in to the above allocation
      mergeAllAllocationsTo(tailSteps, allocation, allocOffset)

    case (nestingStep : ps.NestingStep) :: tailSteps =>
      // It would only be possible to merge allocations across branches if they
      // both allocate the exact same number of cells. Otherwise one branch
      // would have extra uninitialized cells which would confuse our GC.

      // Another option would be to allocate the minimum number of allocations
      // and have the larger branch allocate the difference

      // Both of these seem to have high complexity for the benefit they'd create

      // Abort and switch back to searching both branches and the remaining steps
      val newStep = nestingStep.mapInnerBranches { (steps, _) =>
        findNextAllocation(steps)
      }

      val steps = newStep :: findNextAllocation(tailSteps)

      MergeAllResult(
        requiredSize=allocOffset,
        steps=steps
      )

    case (barrier : ps.GcBarrier) :: tailSteps =>
      // We can't keep uninitialized cells across a GC barrier
      // Abort and switch back to searching
      val steps = findNextAllocation(barrier :: tailSteps)
      
      MergeAllResult(
        requiredSize=allocOffset,
        steps=steps
      )

    case other :: tailSteps =>
      // Pass this step through unmodified
      val tailResult = mergeAllAllocationsTo(tailSteps, allocation, allocOffset)
      
      tailResult.copy(
        steps=other :: tailResult.steps
      )

    case Nil =>
      MergeAllResult(
        requiredSize=allocOffset,
        steps=Nil
      )
  }

  private def findNextAllocation(steps : List[ps.Step]) : List[ps.Step] = steps match {
    // We don't care which size they requested; we'll recompute
    case ps.AllocateCells(allocation, _) :: tailSteps =>
      val mergeResult = mergeAllAllocationsTo(tailSteps, allocation, 0)

      ps.AllocateCells(allocation, mergeResult.requiredSize) :: mergeResult.steps

    case (nestingStep : ps.NestingStep) :: tailSteps =>
      val newStep = nestingStep.mapInnerBranches { (steps, _) =>
        findNextAllocation(steps)
      }

      newStep :: findNextAllocation(tailSteps)

    case other :: tailSteps =>
      other :: findNextAllocation(tailSteps)

    case Nil =>
      Nil
  }

  def conniveFunction(function : PlannedFunction) : PlannedFunction = {
    function.copy(
      steps=findNextAllocation(function.steps)
    )
  }
}
