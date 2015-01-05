package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.dialect
import llambda.compiler.analyser.AnalysedExprs

case class PlanConfig(
  schemeDialect : dialect.Dialect,
  optimise : Boolean,
  analysis : AnalysedExprs
)
