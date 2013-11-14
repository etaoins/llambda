package llambda.analyzer

import llambda.et

object Analyize {
  def apply(exprs : List[et.Expression]) : AnalysisResult = {
    // Find all mutables vars
    val mutableVars = exprs.flatMap(FindMutableVars.apply).toSet

    AnalysisResult(
      mutableVars=mutableVars
    )
  }
}


