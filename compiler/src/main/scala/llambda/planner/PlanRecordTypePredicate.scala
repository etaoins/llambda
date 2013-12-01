package llambda.planner

import llambda.et
import llambda.nfi
import llambda.{valuetype => vt}
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}

object PlanRecordTypePredicate {
  def apply(expr : et.RecordTypePredicate)(implicit parentPlan : PlanWriter) : PlannedFunction = 
    expr match {
      case et.RecordTypePredicate(recordDataType) =>
        // Determine our signature
        val predicateSignature = new nfi.NativeSignature {
          val hasClosureArg : Boolean = false
          val hasRestArg : Boolean = false

          // We must be able to take any data type without erroring out
          val fixedArgs : List[vt.ValueType] = List(vt.BoxedValue(bt.BoxedDatum))
          val returnType : Option[vt.ValueType] = Some(vt.BoxedValue(bt.BoxedRecord))
        }
        
        // We only have a single argument
        val argumentTemp = new ps.TempValue
        
        val plan = parentPlan.forkPlan()

        // Try to cast to a generic record type first
        val isRecordPred = new ps.TempValue
        plan.steps += ps.TestBoxedType(isRecordPred, argumentTemp, bt.BoxedRecord) 

        val retValueTemp = plan.buildCondBranch(isRecordPred, 
          {isRecordPlan =>
            // Cast the boxed type to a boxed record
            val boxedRecordTemp = new ps.TempValue
            isRecordPlan.steps += ps.CastBoxedToTypeUnchecked(boxedRecordTemp, argumentTemp, bt.BoxedRecord)

            val classMatchedPred = new ps.TempValue
            isRecordPlan.steps += ps.TestBoxedRecordClass(classMatchedPred, boxedRecordTemp, recordDataType) 

            val classMatchedBool = new ps.TempValue
            isRecordPlan.steps += ps.ConvertNativeInteger(classMatchedBool, classMatchedPred, nfi.CBool.bits, false)

            classMatchedBool
          },
          {isNotRecordPlan =>
            val falseBool = new ps.TempValue
            isNotRecordPlan.steps += ps.StoreNativeInteger(falseBool, 0, nfi.CBool.bits)

            falseBool
          })

        // Return the phi'ed value
        plan.steps += ps.Return(Some(retValueTemp))

        PlannedFunction(
          signature=predicateSignature,
          namedArguments=List(("value" -> argumentTemp)),
          steps=plan.steps.toList
        )
    }
}
