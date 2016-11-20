package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.analyser.AnalysedExprs

case class PlanConfig(
  optimise : Boolean,
  analysis : AnalysedExprs
)
