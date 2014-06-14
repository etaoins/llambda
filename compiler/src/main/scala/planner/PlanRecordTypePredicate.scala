package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

object PlanRecordTypePredicate {
  def apply(expr : et.RecordTypePredicate)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypePredicate(recordType) =>
        // Determine our signature
        val predicateSignature = ProcedureSignature(
          hasWorldArg=false,
          hasSelfArg=false,
          hasRestArg=false,
          // We must be able to take any data type without erroring out
          fixedArgs=List(vt.IntrinsicCellType(ct.DatumCell)),
          returnType=Some(vt.CBool),
          attributes=Set()
        )
        
        // We only have a single argument
        val argumentTemp = ps.CellTemp(ct.DatumCell)
        
        val plan = parentPlan.forkPlan()

        // Try to cast to a generic record type first
        val isRecordPred = ps.Temp(vt.Predicate)
        plan.steps += ps.TestCellType(isRecordPred, argumentTemp, ct.RecordCell) 

        val retValueTemp = plan.buildCondBranch(isRecordPred, 
          {isRecordPlan =>
            // Cast the boxed type to a boxed record
            val recordCellTemp = ps.RecordTemp()
            isRecordPlan.steps += ps.CastCellToTypeUnchecked(recordCellTemp, argumentTemp, ct.RecordCell)

            val classMatchedPred = ps.Temp(vt.Predicate)
            isRecordPlan.steps += ps.TestRecordLikeClass(classMatchedPred, recordCellTemp, recordType) 

            val classMatchedBool = ps.Temp(vt.CBool)
            isRecordPlan.steps += ps.ConvertNativeInteger(classMatchedBool, classMatchedPred, vt.CBool.bits, false)

            classMatchedBool
          },
          {isNotRecordPlan =>
            val falseBool = ps.Temp(vt.CBool)
            isNotRecordPlan.steps += ps.CreateNativeInteger(falseBool, 0, vt.CBool.bits)

            falseBool
          })

        // Return the phi'ed value
        plan.steps += ps.Return(Some(retValueTemp))

        PlannedFunction(
          signature=predicateSignature,
          namedArguments=List(("value" -> argumentTemp)),
          steps=plan.steps.toList,
          worldPtrOpt=None,
          debugContextOpt=None
        )
    }
}
