package llambda.analyzer

import llambda.StorageLocation

case class AnalysisResult(
  mutableVars : Set[StorageLocation]
)
