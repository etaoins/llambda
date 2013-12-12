package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.analyzer.AnalysisResult

case class PlanConfig(
  optimize : Boolean,
  analysis :  AnalysisResult
)
