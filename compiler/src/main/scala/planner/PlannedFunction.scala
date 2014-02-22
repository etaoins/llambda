package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.planner.{step => ps}

case class PlannedFunction(
  signature : ProcedureSignature,
  namedArguments : List[(String,  ps.TempValue)],
  steps : List[ps.Step],
  worldPtrOption : Option[ps.WorldPtrValue]
)
