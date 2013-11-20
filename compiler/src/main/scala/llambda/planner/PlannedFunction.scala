package llambda.planner

import llambda.nfi
import llambda.planner.{step => ps}

case class PlannedFunction(
  signature : nfi.NativeSignature,
  steps : List[ps.Step]
)
