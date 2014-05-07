package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

case class ReduceConfig(
  analysis : analyzer.AnalysisResult,
  knownConstants : Map[StorageLocation, et.Expression] = Map(),
  // This is used by LiteralValue() to prevent infinite recursion with (letrec)ed variables
  resolvingInitializers : Set[StorageLocation] = Set()
) {
  def constantExprForStorageLoc(storageLoc : StorageLocation) : Option[et.Expression] = 
    knownConstants.get(storageLoc) orElse analysis.constantVars.get(storageLoc)
}
