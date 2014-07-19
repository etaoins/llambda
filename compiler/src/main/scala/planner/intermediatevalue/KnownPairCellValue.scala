package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

/** Represents a constructed pair with a car and cdr value known at compile time
  *
  * These can only exist in Scheme dialects with immutable pairs. In the R7RS dialect these will not be constructed
  */
  class KnownPairCellValue(val car : IntermediateValue, val cdr : IntermediateValue, tempValue : ps.TempValue) extends CellValue(vt.PairType, ct.PairCell, tempValue) with KnownPair {
}
  

