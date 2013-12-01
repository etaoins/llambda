package llambda.planner

import llambda.et
import llambda.nfi
import llambda.{valuetype => vt}
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}

object PlanRecordTypeConstructor {
  def apply(expr : et.RecordTypeConstructor)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeConstructor(recordDataType, initializedFields) =>
        // Determine our signature
        val constructorSignature = new nfi.NativeSignature {
          val hasClosureArg : Boolean = false
          val hasRestArg : Boolean = false

          val fixedArgs : List[vt.ValueType] = initializedFields.map(_.fieldType)
          val returnType : Option[vt.ValueType] = Some(vt.BoxedRecordType(recordDataType))
        }
        
        val plan = parentPlan.forkPlan()

        val fieldToTempValue = (recordDataType.fields.map { field =>
          (field, new ps.TempValue)
        }).toMap

        // Get unique argument names
        val argumentUniquer = new SourceNameUniquer
        val namedArguments = (fieldToTempValue.map { case (field, tempValue) =>
          (argumentUniquer(field.sourceName) -> tempValue)
        }).toList

        // Make our bare record
        val recordDataTemp = new ps.TempValue 

        plan.steps += ps.RecordAllocate(recordDataTemp, recordDataType)
        
        // Set all our fields
        for(field <- initializedFields) {
          plan.steps += ps.RecordFieldSet(recordDataTemp, recordDataType, field, fieldToTempValue(field))
        }

        // Box the result
        val allocation = new ps.TempAllocation
        val boxedTemp = new ps.TempValue

        plan.steps += ps.AllocateCons(allocation, 1)
        plan.steps += ps.BoxRecord(boxedTemp, allocation, 0, recordDataType, recordDataTemp)
        plan.steps += ps.Return(Some(boxedTemp))

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=namedArguments,
          steps=plan.steps.toList
        )
    }
}
