package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.BoxedValue

/** Represents a constructed vector with a length known at compile time */
class KnownVectorCellValue(
    val vectorLength: Long,
    tempValue: ps.TempValue
) extends CellValue(vt.VectorType, BoxedValue(ct.VectorCell, tempValue)) with KnownVector {
  override def withSchemeType(newType: vt.SchemeType): IntermediateValue =
    this
}
