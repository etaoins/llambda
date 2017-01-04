package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

object WorldPtrUsedBySteps {
  private def worldPtrUsedByStep(step: ps.Step): Boolean = step match {
    case nestingStep: ps.NestingStep =>
      // Recurse down each side
      nestingStep.outerInputValues.contains(ps.WorldPtrValue) ||
        nestingStep.innerBranches.flatMap(_._1).exists(worldPtrUsedByStep)

    case otherStep =>
      // This sucks - there are no explicit allocation steps until PlanCellAllocations runs which is the last phase of
      // planning. We have to implicitly know heap consuming steps will generate steps requiring the world pointer
      (otherStep.requiredHeapCells > 0) || otherStep.inputValues.contains(ps.WorldPtrValue)
  }

  def apply(steps: List[ps.Step]): Boolean =
    steps.exists(worldPtrUsedByStep)
}
