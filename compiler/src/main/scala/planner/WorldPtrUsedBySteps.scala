package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

object WorldPtrUsedBySteps {
  private def worldPtrUsedByStep(step: ps.Step): Boolean = step match {
    case condBranch: ps.CondBranch =>
      // Recurse down each side
      condBranch.outerInputValues.contains(ps.WorldPtrValue) ||
        condBranch.innerBranches.flatMap(_._1).exists(worldPtrUsedByStep)

    case otherStep =>
      // This sucks - there are no explicit allocation steps until PlanHeapAllocations runs which is the last phase of
      // planning. We have to implicitly know heap consuming steps will generate steps requiring the world pointer
      (otherStep.requiredHeapCells > 0) || otherStep.inputValues.contains(ps.WorldPtrValue)
  }

  def apply(steps: List[ps.Step]): Boolean =
    steps.exists(worldPtrUsedByStep)
}
