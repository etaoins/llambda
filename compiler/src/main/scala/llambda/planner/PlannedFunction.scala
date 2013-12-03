package llambda.planner

import llambda.ProcedureSignature
import llambda.planner.{step => ps}

case class PlannedFunction(
  signature : ProcedureSignature,
  namedArguments : List[(String,  ps.TempValue)],
  steps : List[ps.Step]
)
