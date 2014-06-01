package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer.{partialvalue => pv}

object ReduceExprs {
  def apply(analysis : AnalysedExprs) : et.Expr = {
    val exprs = analysis.usedTopLevelExprs

    val topLevelReduceConfig = ReduceConfig(
      analysis=analysis
    )

    // Pre-reduce our top level definitions to make them known valeus
    // This prevents constant re-evaluation of top-level defines
    val usedTopLevelBindings = analysis.constantTopLevelBindings.filter { case (storageLoc, _) =>
      analysis.usedVars.contains(storageLoc)
    }

    val initialKnownValues = usedTopLevelBindings.mapValues { initializer =>
      pv.PartialValue.fromReducedExpr(
        ReduceExpr(initializer)(topLevelReduceConfig)
      )
    }

    val reduceConfig = topLevelReduceConfig.copy(
      knownValues=initialKnownValues
    )

    ReduceExpr(et.Begin(exprs))(reduceConfig)
  }
}
