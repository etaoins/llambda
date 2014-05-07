package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

object ReduceExpressions {
  def apply(exprs : List[et.Expression])(analysis : analyzer.AnalysisResult) : et.Expression = {
    val reduceConfig = ReduceConfig(
      analysis=analysis
    )

    ReduceExpression(et.Begin(exprs))(reduceConfig)
  }
}
