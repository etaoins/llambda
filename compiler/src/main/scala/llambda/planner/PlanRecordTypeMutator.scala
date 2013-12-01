package llambda.planner

import llambda.et
import llambda.nfi
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}

object PlanRecordTypeMutator {
  def apply(expr : et.RecordTypeMutator)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeMutator(recordDataType, field) =>
        // Determine our signature
        val constructorSignature = new nfi.NativeSignature {
          val hasClosureArg : Boolean = false
          val hasRestArg : Boolean = false

          val fixedArgs : List[vt.ValueType] = List(recordDataType, field.fieldType)
          val returnType : Option[vt.ValueType] = None
        }

        // Set up our arguments
        val recordDataTemp = new ps.TempValue
        val newValueTemp = new ps.TempValue

        val namedArguments = List(
          ("recordData" -> recordDataTemp),
          ("newValue" -> newValueTemp)
        )
        
        val plan = parentPlan.forkPlan()

        val fieldValueTemp = new ps.TempValue
        plan.steps += ps.RecordFieldSet(recordDataTemp, recordDataType, field, newValueTemp) 

        plan.steps += ps.Return(None)

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=namedArguments,
          steps=plan.steps.toList
        )
    }
}


