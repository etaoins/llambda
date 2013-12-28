package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

object PlanRecordTypeConstructor {
  def apply(expr : et.RecordTypeConstructor)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypeConstructor(recordType, initializedFields) =>
        // Determine our signature
        val constructorSignature = new ProcedureSignature {
          val hasSelfArg : Boolean = false
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
        
        // Allocate the cell
        val allocation = new ps.TempAllocation
        plan.steps += ps.AllocateCells(allocation, 1)

        // Initialize the record
        val cellTemp = new ps.TempValue 
        val dataTemp = new ps.TempValue 

        plan.steps += ps.RecordLikeInit(cellTemp, dataTemp, allocation, 0, recordType)
        
        // Set all our fields
        for(field <- recordType.fields) {
          val fieldTemp = if (initializedFields.contains(field)) {
            fieldToTempValue(field)
          }
          else {
            iv.UnspecificValue.toTempValue(field.fieldType)(plan)
          }
            
          plan.steps += ps.RecordDataFieldSet(dataTemp, recordType, field, fieldTemp)
        }

        // Return the record
        plan.steps += ps.Return(Some(cellTemp))

        PlannedFunction(
          signature=constructorSignature,
          namedArguments=namedArguments,
          steps=plan.steps.toList
        )
    }
}
