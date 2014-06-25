package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

object WorldPtrUsedBySteps {
  private def worldPtrUsedByStep(worldPtrTemp : ps.TempValue, step : ps.Step) : Boolean = step match {
    case consumer : ps.CellConsumer =>
      // This sucks - there are no explicit allocation steps until PlanCellAllocations runs which is the last phase of
      // planning. We have to implicitly know CellConsumers will generate steps requiring the world pointer
      true

    case condStep : ps.CondBranch =>
      // Recurse down each side
      condStep.outerInputValues.contains(worldPtrTemp) || 
        condStep.innerBranches.flatMap(_._1).exists({ branchStep =>
          worldPtrUsedByStep(worldPtrTemp, branchStep)
        })

    case otherStep =>
      otherStep.inputValues.contains(worldPtrTemp)
  }

  def apply(steps : List[ps.Step], worldPtrTemp : ps.WorldPtrValue) : Boolean = 
    steps.exists(worldPtrUsedByStep(worldPtrTemp, _))
}
