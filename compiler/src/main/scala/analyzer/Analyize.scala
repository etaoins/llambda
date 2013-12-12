package io.llambda.compiler.analyzer
import io.llambda

import llambda.compiler.et

object Analyize {
  def apply(exprs : List[et.Expression]) : AnalysisResult = {
    // Find all mutables vars
    val mutableVars = exprs.flatMap(FindMutableVars.apply).toSet

    AnalysisResult(
      mutableVars=mutableVars
    )
  }
}


