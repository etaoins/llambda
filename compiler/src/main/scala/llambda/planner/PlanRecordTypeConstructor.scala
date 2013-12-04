package llambda.planner

import llambda.et
import llambda.ProcedureSignature
import llambda.{valuetype => vt}
import llambda.{celltype => ct}
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}

object PlanRecordTypeConstructor {
  def apply(expr : et.RecordTypeConstructor)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeConstructor(recordType, initializedFields) =>
        // Determine our signature
        val constructorSignature = new ProcedureSignature {
          val hasClosureArg : Boolean = false
          val hasRestArg : Boolean = false

          val fixedArgs : List[vt.ValueType] = initializedFields.map(_.fieldType)
          val returnType : Option[vt.ValueType] = Some(recordType)
        }
        
        val plan = parentPlan.forkPlan()

        val fieldToTempValue = (recordType.fields.map { field =>
          (field, new ps.TempValue)
        }).toMap

        // Get unique argument names
        val argumentUniquer = new SourceNameUniquer
        val namedArguments = (initializedFields.map { case field =>
          (argumentUniquer(field.sourceName) -> fieldToTempValue(field))
        }).toList

        // Make our bare record
        val recordDataTemp = new ps.TempValue 

        plan.steps += ps.RecordDataAllocate(recordDataTemp, recordType)
        
        // Set all our fields
        for(field <- recordType.fields) {
          val fieldTemp = if (initializedFields.contains(field)) {
            fieldToTempValue(field)
          }
          else {
            iv.UnspecificValue.toRequiredTempValue(field.fieldType)(plan)
          }
            
          plan.steps += ps.RecordFieldSet(recordDataTemp, recordType, field, fieldTemp)

        }

        // Box the result
        val allocation = new ps.TempAllocation
        val boxedTemp = new ps.TempValue

        plan.steps += ps.AllocateCells(allocation, 1)
        plan.steps += ps.BoxRecord(boxedTemp, allocation, 0, recordType, recordDataTemp)
        plan.steps += ps.Return(Some(boxedTemp))

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=namedArguments,
          steps=plan.steps.toList
        )
    }
}
