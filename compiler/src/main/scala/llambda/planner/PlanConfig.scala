package llambda.planner

import llambda.analyzer.AnalysisResult

case class PlanConfig(
  optimize : Boolean,
  analysis :  AnalysisResult
)
