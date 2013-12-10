package llambda.planner

import llambda.et
import llambda.ProcedureSignature
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}

object PlanRecordTypeAccessor {
  def apply(expr : et.RecordTypeAccessor)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeAccessor(recordType, field) =>
        // Determine our signature
        val constructorSignature = new ProcedureSignature {
          val hasSelfArg : Boolean = false
          val hasRestArg : Boolean = false

          val fixedArgs : List[vt.ValueType] = List(recordType)
          val returnType : Option[vt.ValueType] = Some(field.fieldType)
        }

        val recordCellTemp = new ps.TempValue
        
        val plan = parentPlan.forkPlan()

        // Extract the record data
        val recordDataTemp = new ps.TempValue
        plan.steps += ps.StoreRecordLikeData(recordDataTemp, recordCellTemp, recordType) 
        
        // Read the field
        val fieldValueTemp = new ps.TempValue
        plan.steps += ps.RecordDataFieldRef(fieldValueTemp, recordDataTemp, recordType, field) 

        plan.steps += ps.Return(Some(fieldValueTemp))

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=List(("recordCell" -> recordCellTemp)),
          steps=plan.steps.toList
        )
    }
}

