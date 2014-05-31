package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer.{partialvalue => pv}

object ReduceExpressions {
  def apply(analysis : AnalysedExpressions) : et.Expression = {
    val exprs = analysis.usedTopLevelExpressions

    val topLevelReduceConfig = ReduceConfig(
      analysis=analysis
    )

    // Pre-reduce our top level definitions to make them known valeus
    // This prevents constant re-evaluation of top-level defines
    val usedTopLevelBindings = analysis.constantTopLevelBindings.filter { case (storageLoc, _) =>
      analysis.usedVars.contains(storageLoc)
    }

    val initialKnownValues = usedTopLevelBindings.mapValues { initializer =>
      pv.PartialValue.fromReducedExpression(
        ReduceExpression(initializer)(topLevelReduceConfig)
      )
    }

    val reduceConfig = topLevelReduceConfig.copy(
      knownValues=initialKnownValues
    )

    ReduceExpression(et.Begin(exprs))(reduceConfig)
  }
}
