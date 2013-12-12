package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlanWriter

class RecordValue(val recordType : vt.RecordType, val tempValue : ps.TempValue) extends IntermediateCellValue with UninvokableValue {
  val possibleTypes = Set[ct.ConcreteCellType](ct.RecordCell)
  val cellType = ct.RecordCell

  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    // Records have no bative representation
    None
  
  def toRecordTempValue(targetRecordType : vt.RecordType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    if (recordType == targetRecordType) {
      // We're of the correct type
      Some(tempValue)
    }
    else {
      // Not of the correct type
      None
    }
  
  def preferredRepresentation : vt.ValueType =
    recordType
  
  def closureRepresentation : Option[vt.ValueType] = 
    Some(recordType)
}
