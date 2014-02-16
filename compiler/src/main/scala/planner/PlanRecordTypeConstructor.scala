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
        val constructorSignature = ProcedureSignature(
          hasSelfArg =false,
          hasRestArg =false,
          fixedArgs=initializedFields.map(_.fieldType),
          returnType=Some(recordType)
        )
        
        val plan = parentPlan.forkPlan()

        val fieldToTempValue = (recordType.fields.map { field =>
          (field, new ps.TempValue(field.fieldType.isGcManaged))
        }).toMap

        // Get unique argument names
        val argumentUniquer = new SourceNameUniquer
        val namedArguments = (initializedFields.map { case field =>
          (argumentUniquer(field.sourceName) -> fieldToTempValue(field))
        }).toList
        
        // Initialize the record
        val cellTemp = ps.GcManagedValue()
        val dataTemp = ps.GcUnmanagedValue()

        plan.steps += ps.RecordLikeInit(cellTemp, dataTemp, recordType)
        
        // Set all our fields
        for(field <- recordType.fields) {
          val fieldTemp = if (initializedFields.contains(field)) {
            fieldToTempValue(field)
          }
          else {
            iv.UnitValue.toTempValue(field.fieldType)(plan)
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
