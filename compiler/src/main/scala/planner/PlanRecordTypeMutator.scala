package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

object PlanRecordTypeMutator {
  def apply(expr : et.RecordTypeMutator)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeMutator(recordType, field) =>
        // Determine our signature
        val constructorSignature = new ProcedureSignature {
          val hasSelfArg : Boolean = false
          val hasRestArg : Boolean = false

          val fixedArgs : List[vt.ValueType] = List(recordType, field.fieldType)
          val returnType : Option[vt.ValueType] = None
        }

        // Set up our arguments
        val recordCellTemp = ps.GcManagedValue()
        val newValueTemp = new ps.TempValue(field.fieldType.isGcManaged)

        val namedArguments = List(
          ("recordCell" -> recordCellTemp),
          ("newValue" -> newValueTemp)
        )
        
        val plan = parentPlan.forkPlan()
        
        // Extract the record data
        val recordDataTemp = ps.GcUnmanagedValue()
        plan.steps += ps.StoreRecordLikeData(recordDataTemp, recordCellTemp, recordType) 

        // Store the new value
        plan.steps += ps.RecordDataFieldSet(recordDataTemp, recordType, field, newValueTemp) 

        // Dispose of the record data pointer
        plan.steps += ps.DisposeValue(recordDataTemp)

        plan.steps += ps.Return(None)

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=namedArguments,
          steps=plan.steps.toList
        )
    }
}


