package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.reducer.AnalysedExpressions

case class PlanConfig(
  optimize : Boolean,
  analysis :  AnalysedExpressions
)
