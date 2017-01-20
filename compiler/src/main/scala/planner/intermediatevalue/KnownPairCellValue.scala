package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.BoxedValue

/** Represents a constructed pair with a car and cdr value known at compile time */
class KnownPairCellValue(
    val car: IntermediateValue,
    val cdr: IntermediateValue,
    schemeType: vt.PairType,
    tempValue: ps.TempValue
) extends CellValue(schemeType, BoxedValue(ct.PairCell, tempValue)) with KnownPair {
  override def withSchemeType(newType: vt.SchemeType): IntermediateValue =
    // Our type is well specified; it's unlikely the type constraint system can help us much here
    this
}

