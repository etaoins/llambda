package llambda.planner

import llambda.et
import llambda.nfi
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}

object PlanRecordTypeAccessor {
  def apply(expr : et.RecordTypeAccessor)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeAccessor(recordDataType, field) =>
        // Determine our signature
        val constructorSignature = new nfi.NativeSignature {
          val hasClosureArg : Boolean = false
          val hasRestArg : Boolean = false

          val fixedArgs : List[vt.ValueType] = List(recordDataType)
          val returnType : Option[vt.ValueType] = Some(field.fieldType)
        }
        
        val recordDataTemp = new ps.TempValue
        
        val plan = parentPlan.forkPlan()

        val fieldValueTemp = new ps.TempValue
        plan.steps += ps.RecordFieldRef(fieldValueTemp, recordDataTemp, recordDataType, field) 

        plan.steps += ps.Return(Some(fieldValueTemp))

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=List(("recordData" -> recordDataTemp)),
          steps=plan.steps.toList
        )
    }
}

