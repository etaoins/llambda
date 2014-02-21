package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.RuntimeErrorMessage

class RecordValue(val recordType : vt.RecordType, val tempValue : ps.TempValue) extends IntermediateCellValue with UninvokableValue {
  val possibleTypes = Set[ct.ConcreteCellType](ct.RecordCell)
  val cellType = ct.RecordCell

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    // Records have no native representation
    impossibleConversion(s"Cannot convert record of type ${recordType.schemeName} to requested type ${nativeType.schemeName} or any other native type")
  
  def toRecordTempValue(targetRecordType : vt.RecordType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    if (recordType == targetRecordType) {
      // We're of the correct type
      tempValue
    }
    else {
      // Not of the correct type
      impossibleConversion(s"Cannot convert record of type ${recordType.sourceName} to record of type ${targetRecordType.sourceName}")
    }
  
  def preferredRepresentation : vt.ValueType =
    recordType
  
  def closureRepresentation : Option[vt.ValueType] = 
    Some(recordType)
}
