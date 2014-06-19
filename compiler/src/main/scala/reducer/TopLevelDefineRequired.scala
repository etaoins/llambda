package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler.StorageLocation
import llambda.compiler.et

object TopLevelDefineRequired {
  def apply(storageLoc : StorageLocation, initialiser : et.Expr, analysis : AnalysedExprs) : Boolean =
    // Is this value used anywhere?
    analysis.usedVars.contains(storageLoc) ||
      // Do we need to keep ths around for a later mutable set operation?
      analysis.mutableVars.contains(storageLoc) ||
      // Does this need an explicit type check?
      storageLoc.hasTypeConstraints ||
      // Does the initialiser have side effects
      ExprHasSideEffects(initialiser)
}
