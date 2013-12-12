package io.llambda.compiler.analyzer
import io.llambda

import llambda.compiler.StorageLocation

case class AnalysisResult(
  mutableVars : Set[StorageLocation]
)
