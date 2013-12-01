package llambda.planner

import llambda.et
import llambda.nfi
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}

object PlanRecordTypeAccessor {
  def apply(expr : et.RecordTypeAccessor)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeAccessor(recordType, field) =>
        // Determine our signature
        val constructorSignature = new nfi.NativeSignature {
          val hasClosureArg : Boolean = false
          val hasRestArg : Boolean = false

          val fixedArgs : List[vt.ValueType] = List(recordType)
          val returnType : Option[vt.ValueType] = Some(field.fieldType)
        }

        val boxedRecordTemp = new ps.TempValue
        
        val plan = parentPlan.forkPlan()

        // Extract the record data
        val recordDataTemp = new ps.TempValue
        plan.steps += ps.StoreBoxedRecordData(recordDataTemp, boxedRecordTemp, recordType) 
        
        // Read the field
        val fieldValueTemp = new ps.TempValue
        plan.steps += ps.RecordFieldRef(fieldValueTemp, recordDataTemp, recordType, field) 

        plan.steps += ps.Return(Some(fieldValueTemp))

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=List(("boxedRecord" -> boxedRecordTemp)),
          steps=plan.steps.toList
        )
    }
}

