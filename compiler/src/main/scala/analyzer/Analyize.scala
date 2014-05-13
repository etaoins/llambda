package io.llambda.compiler.analyzer
import io.llambda

import llambda.compiler.et

object Analyize {
  def apply(exprs : List[et.Expression]) : AnalysisResult = {
    // Find all mutables vars
    val foundVars = FindVars(et.Begin(exprs))

    // We don't care about the initializers of mutable variables
    val constantTopLevelBindings = foundVars.topLevelBindings -- foundVars.mutableVars

    AnalysisResult(
      mutableVars=foundVars.mutableVars,
      constantTopLevelBindings=constantTopLevelBindings,
      usedVars=foundVars.usedVars
    )
  }
}


