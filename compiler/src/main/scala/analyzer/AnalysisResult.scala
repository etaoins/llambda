package io.llambda.compiler.analyzer
import io.llambda

import llambda.compiler.{StorageLocation, et}

case class AnalysisResult(
  mutableVars : Set[StorageLocation],
  constantInitializers : Map[StorageLocation, et.Expression],
  usedVars : Set[StorageLocation]
)
