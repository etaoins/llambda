package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}

private[planner] object ValuesToRecord {
  private def valuesToMutableRecordTemp(
    recordType: vt.RecordType,
    fieldValues: Map[vt.RecordField, iv.IntermediateValue],
    isUndefined: Boolean = false
  )(implicit plan: PlanWriter): ps.TempValue = {
    // Convert our fields to temp values
    val fieldToTempValue = fieldValues.map { case (field, value) =>
      val fieldType = recordType.typeForField(field)
      field -> value.toTempValue(fieldType)
    }

    val cellTemp = ps.RecordTemp()
    plan.steps += ps.InitRecord(cellTemp, recordType, fieldToTempValue, isUndefined=isUndefined)
    cellTemp
  }

  /** Converts fields values to the specified record type
    *
    * This will construct constant records where possible and will otherwise create a KnownRecordCellValue instances
    * with all known immutable fields
    *
    * @param  recordType   Record type to construct
    * @param  fieldValues  Values for each record field. This must be exhaustive; defaults won't be set implicitly
    * @param  isUndefined  Indicates if the record should be initially marked as undefined
    */
  def apply(
    recordType: vt.RecordType,
    fieldValues: Map[vt.RecordField, iv.IntermediateValue],
    isUndefined: Boolean = false
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
    val immutableFieldValues = fieldValues.filter(!_._1.mutable)

    // Are all of our fields immutable and constant?
    val constantImmutableFieldValues = immutableFieldValues collect {
      case (field, value: iv.ConstantValue) if !field.mutable =>
        // Make sure it's the right type
        val fieldType = recordType.typeForField(field)
        value.castToSchemeType(fieldType.schemeType)

        (field -> value)
    }

    if (constantImmutableFieldValues.size == fieldValues.size) {
      new iv.ConstantRecordValue(recordType, constantImmutableFieldValues, isUndefined)
    }
    else {
      val recordTemp = valuesToMutableRecordTemp(recordType, fieldValues, isUndefined)
      new iv.KnownRecordCellValue(immutableFieldValues, recordType, recordTemp)
    }
  }
}
