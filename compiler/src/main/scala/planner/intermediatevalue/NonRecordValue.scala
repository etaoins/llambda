package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlanWriter

/** Trait for IntermediateValues that cannot represent records */
trait NonRecordValue extends IntermediateValue {
  protected def toRecordTempValue(recordType : vt.RecordType)(implicit plan : PlanWriter) : Option[ps.TempValue] = None
}

