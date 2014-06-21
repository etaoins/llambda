package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.RuntimeErrorMessage

class RecordValue(val recordType : vt.RecordType, val tempValue : ps.TempValue) extends IntermediateCellValue with UninvokableValue with BoxedOnlyValue {
  val possibleTypes = Set[ct.ConcreteCellType](ct.RecordCell)
  val cellType = ct.RecordCell

  override lazy val typeDescription = s"record of type ${recordType.schemeName}"

  def toRecordTempValue(targetRecordType : vt.RecordType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    if (recordType == targetRecordType) {
      // We're of the correct type
      tempValue
    }
    else {
      // Not of the correct type
      impossibleConversion(s"Cannot convert ${typeDescription} to record of distinct type ${targetRecordType.sourceName}")
    }
  
  protected def hasRecordType(testingType : vt.RecordType) =
    Some(testingType == recordType)
  
  def preferredRepresentation : vt.ValueType =
    recordType
  
  def needsClosureRepresentation =
    true
}
