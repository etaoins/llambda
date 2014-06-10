package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.RuntimeErrorMessage

/** Trait for IntermediateValues that cannot represent records */
trait NonRecordValue extends IntermediateValue {
  protected def toRecordTempValue(recordType : vt.RecordType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = 
    impossibleConversion(s"Cannot convert ${typeDescription} to record type ${recordType.schemeName}")
}

