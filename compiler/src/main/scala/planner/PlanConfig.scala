package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.reducer.AnalysedExprs

case class PlanConfig(
  optimize : Boolean,
  analysis :  AnalysedExprs
)
