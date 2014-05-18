package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

object PlanRecordTypeAccessor {
  def apply(expr : et.RecordTypeAccessor)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeAccessor(recordType, field) =>
        // Determine our signature
        val accessorSignature = ProcedureSignature(
          hasWorldArg=false,
          hasSelfArg=false,
          hasRestArg=false,
          fixedArgs=List(recordType),
          returnType=Some(field.fieldType)
        )

        val recordCellTemp = ps.RecordTemp()
        
        val plan = parentPlan.forkPlan()

        // Extract the record data
        val recordDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.StoreRecordLikeData(recordDataTemp, recordCellTemp, recordType) 
        
        // Read the field
        val fieldValueTemp = new ps.TempValue(field.fieldType.isGcManaged)
        plan.steps += ps.RecordDataFieldRef(fieldValueTemp, recordDataTemp, recordType, field) 

        // Dispose of the record data pointer
        plan.steps += ps.DisposeValue(recordDataTemp)

        plan.steps += ps.Return(Some(fieldValueTemp))

        PlannedFunction(
          signature=accessorSignature,
          namedArguments=List(("recordCell" -> recordCellTemp)),
          steps=plan.steps.toList,
          None
        )
    }
}

