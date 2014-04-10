package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, StorageLocation, ReportProcedure, SourceLocated}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{intermediatevalue => iv}

private[planner] object PlanBind {
  def apply(initialState : PlannerState)(bindings : List[(StorageLocation, et.Expression)])(implicit planConfig : PlanConfig, plan : PlanWriter) : PlannerState = {
    implicit val worldPtr = initialState.worldPtr

    bindings.foldLeft(initialState) { case (state, (storageLoc, initialValue)) =>
      if (planConfig.analysis.mutableVars.contains(storageLoc)) {
        val mutableTemp = ps.GcManagedValue()
        
        val initialValueResult = PlanExpression(state)(initialValue)
        val initialValueTemp = initialValueResult.value.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

        // Create a new mutable
        val recordDataTemp = ps.GcUnmanagedValue()
        plan.steps += ps.RecordLikeInit(mutableTemp, recordDataTemp, vt.MutableType)

        // Set the value
        plan.steps += ps.RecordDataFieldSet(recordDataTemp, vt.MutableType, vt.MutableField, initialValueTemp)
    
        plan.steps += ps.DisposeValue(recordDataTemp)
        
        initialValueResult.state.withValue(storageLoc -> MutableValue(mutableTemp))
      }
      else {
        // Send a hint about our name
        val initialValueResult = PlanExpression(state)(initialValue, Some(storageLoc.sourceName))

        val reportNamedValue = (initialValueResult.value, storageLoc) match {
          case (knownProc : iv.KnownProcedure, reportProc : ReportProcedure) =>
            // Annotate with our report name so we can optimize when we try
            // to apply this
            // Note this is agnostic to if the implementation is a native
            // function versus a Scheme procedure
            knownProc.withReportName(reportProc.reportName)

          case (otherValue, _) =>
            otherValue
        }

        // No planning, just remember this intermediate value
        initialValueResult.state.withValue(storageLoc -> ImmutableValue(reportNamedValue))
      }
    }
  }
}
