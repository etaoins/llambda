package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer.{partialvalue => pv}

case class ReduceConfig(
  analysis : AnalysedExprs,
  knownValues : Map[StorageLocation, pv.PartialValue] = Map(),
  // This is used by LiteralValue() to prevent infinite recursion with (letrec)ed variables
  resolvingInitializers : Set[StorageLocation] = Set(),
  inlineDepth : Int = 0
)
