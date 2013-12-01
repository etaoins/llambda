package llambda.planner

import llambda.et
import llambda.nfi
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}

object PlanRecordTypeMutator {
  def apply(expr : et.RecordTypeMutator)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeMutator(recordType, field) =>
        // Determine our signature
        val constructorSignature = new nfi.NativeSignature {
          val hasClosureArg : Boolean = false
          val hasRestArg : Boolean = false

          val fixedArgs : List[vt.ValueType] = List(recordType, field.fieldType)
          val returnType : Option[vt.ValueType] = None
        }

        // Set up our arguments
        val boxedRecordTemp = new ps.TempValue
        val newValueTemp = new ps.TempValue

        val namedArguments = List(
          ("boxedRecord" -> boxedRecordTemp),
          ("newValue" -> newValueTemp)
        )
        
        val plan = parentPlan.forkPlan()
        
        // Extract the record data
        val recordDataTemp = new ps.TempValue
        plan.steps += ps.StoreBoxedRecordData(recordDataTemp, boxedRecordTemp, recordType) 

        val fieldValueTemp = new ps.TempValue
        plan.steps += ps.RecordFieldSet(recordDataTemp, recordType, field, newValueTemp) 

        plan.steps += ps.Return(None)

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=namedArguments,
          steps=plan.steps.toList
        )
    }
}


