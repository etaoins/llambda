
package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.BoxedValue

/** Represents a constructed bytevector with a length known at compile time */
class KnownBytevectorCellValue(
    val bytevectorLength: Long,
    tempValue: ps.TempValue
) extends CellValue(vt.BytevectorType, BoxedValue(ct.BytevectorCell, tempValue)) with KnownBytevector {
  override def withSchemeType(newType: vt.SchemeType): IntermediateValue =
    this
}
