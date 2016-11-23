package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{intermediatevalue => iv}

case class PlanResult(
  state : PlannerState,
  value : ResultValue
)
