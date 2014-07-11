package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.analyser.AnalysedExprs

case class PlanConfig(
  optimize : Boolean,
  analysis : AnalysedExprs
)
