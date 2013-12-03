package llambda.planner.intermediatevalue

import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.PlanWriter

/** Trait for IntermediateValues that cannot represent records */
trait NonRecordValue extends IntermediateValue {
  protected def toRecordCellTempValue(recordCellType : vt.RecordCellType)(implicit plan : PlanWriter) : Option[ps.TempValue] = None
}

