package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.BoxedValue

/** Represents a constructed record cell with zero or more known immutable fields */
class KnownRecordCellValue(
    val knownFieldValues: Map[vt.RecordField, IntermediateValue],
    schemeType: vt.RecordType,
    tempValue: ps.TempValue
) extends CellValue(schemeType, BoxedValue(ct.RecordCell, tempValue)) with KnownRecord {
  override def withSchemeType(newType: vt.SchemeType): IntermediateValue =
    this
}
