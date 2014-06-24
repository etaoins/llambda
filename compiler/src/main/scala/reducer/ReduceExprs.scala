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

    val usedTopLevelBindings = analysis.constantTopLevelBindings.filter { case (storageLoc, _) =>
      analysis.usedVars.contains(storageLoc)
    }

    // Evaluate our top-level bindings in order and store them as known values
    val initialReduceConfig = ReduceConfig(
      analysis=analysis
    )

    val reduceConfig = usedTopLevelBindings.foldLeft(initialReduceConfig) { 
      case (reduceConfig, (storageLoc, initialiser)) =>
        val initialiserPartialValue = pv.PartialValue.fromReducedExpr(
          ReduceExpr(initialiser)(reduceConfig)
        )

        reduceConfig.copy(
          knownValues=reduceConfig.knownValues + (storageLoc -> initialiserPartialValue)
        )
    }

    ReduceExpr(et.Begin(exprs))(reduceConfig)
  }
}
