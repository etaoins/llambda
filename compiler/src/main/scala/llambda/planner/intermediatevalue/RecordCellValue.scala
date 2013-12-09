package llambda.planner.intermediatevalue

import llambda.{valuetype => vt}
import llambda.{celltype => ct}
import llambda.planner.{step => ps}
import llambda.planner.PlanWriter

class RecordCellValue(val recordType : vt.RecordCellType, val tempValue : ps.TempValue) extends IntermediateCellValue with UninvokableValue {
  val possibleTypes = Set[ct.ConcreteCellType](ct.RecordCell)
  val cellType = ct.RecordCell

  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    // Records have no bative representation
    None
  
  def toRecordCellTempValue(targetRecordType : vt.RecordCellType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
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
}
