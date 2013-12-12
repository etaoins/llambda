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
        val recordCellTemp = new ps.TempValue
        val newValueTemp = new ps.TempValue

        val namedArguments = List(
          ("recordCell" -> recordCellTemp),
          ("newValue" -> newValueTemp)
        )
        
        val plan = parentPlan.forkPlan()
        
        // Extract the record data
        val recordDataTemp = new ps.TempValue
        plan.steps += ps.StoreRecordLikeData(recordDataTemp, recordCellTemp, recordType) 

        val fieldValueTemp = new ps.TempValue
        plan.steps += ps.RecordDataFieldSet(recordDataTemp, recordType, field, newValueTemp) 

        plan.steps += ps.Return(None)

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=namedArguments,
          steps=plan.steps.toList
        )
    }
}


