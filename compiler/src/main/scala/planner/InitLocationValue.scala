package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.codegen.CompactRepresentationForType
import llambda.compiler.StorageLocation

import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}


private[planner] object InitLocationValue {
  def apply(storageLoc: StorageLocation, value: iv.IntermediateValue)(implicit plan: PlanWriter): LocationValue = {
    if (plan.config.analysis.mutableVars.contains(storageLoc)) {
      // Init the mutable
      val mutableTemp = ps.TempValue()

      // Determine our type and convert the argument to it
      val compactInnerType = CompactRepresentationForType(storageLoc.schemeType)
      val mutableType = MutableType(compactInnerType)
      val tempValue = value.toTempValue(compactInnerType)

      // Set the value
      val fieldValues = Map[vt.RecordField, ps.TempValue](mutableType.recordField -> tempValue)
      plan.steps += ps.InitRecord(mutableTemp, mutableType, fieldValues, isUndefined=false)

      MutableValue(mutableType, mutableTemp, false)
    }
    else {
      ImmutableValue(value)
    }
  }
}
