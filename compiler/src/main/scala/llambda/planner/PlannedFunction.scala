package llambda.planner

import llambda.nfi
import llambda.planner.{step => ps}

case class PlannedFunction(
  signature : nfi.NativeSignature,
  namedArguments : List[(String,  ps.TempValue)],
  steps : List[ps.Step]
)
