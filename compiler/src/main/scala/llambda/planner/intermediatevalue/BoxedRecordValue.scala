package llambda.planner.intermediatevalue

import llambda.nfi
import llambda.{valuetype => vt}
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.PlanWriter

class BoxedRecordValue(val recordType : vt.BoxedRecordType, val tempValue : ps.TempValue) extends BoxedIntermediateValue with UninvokableValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedRecord)
  val valueType = bt.BoxedRecord

  def toScalarTempValue(unboxedType : nfi.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    // Records have no scalar representation
    None
  
  def toBoxedRecordTempValue(targetRecordType : vt.BoxedRecordType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    if (recordType == targetRecordType) {
      // We're of the correct type
      Some(tempValue)
    }
    else {
      // Not of the correct type
      None
    }
}
