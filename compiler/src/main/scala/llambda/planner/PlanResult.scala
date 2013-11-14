package llambda.planner

import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}

case class PlanResult(
  state : PlannerState,
  value : iv.IntermediateValue
)
