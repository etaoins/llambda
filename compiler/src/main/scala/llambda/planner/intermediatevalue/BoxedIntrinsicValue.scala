package llambda.planner.intermediatevalue

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.{PlanWriter, InvokableProcedure}

class BoxedIntrinsicValue(val possibleTypes : Set[bt.ConcreteBoxedType], val valueType : bt.BoxedType, val tempValue : ps.TempValue) extends BoxedIntermediateValue {
  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val truthyTemp = new ps.TempValue

    if (possibleTypes.contains(bt.BoxedBoolean)) {
      plan.steps += ps.UnboxAsTruthy(truthyTemp, tempValue) 
    }
    else {
      plan.steps += ps.StoreNativeInteger(truthyTemp, 1, 1) 
    }

    truthyTemp
  }
  
  def toInvokableProcedure()(implicit plan : PlanWriter) : Option[InvokableProcedure] =  {
    if (possibleTypes.contains(bt.BoxedProcedure)) {
      // Cast to a procedure
      val boxedProcTmep = toRequiredTempValue(vt.BoxedIntrinsicType(bt.BoxedProcedure))

      Some(new InvokableBoxedProcedure(boxedProcTmep))
    }
    else {
      None
    }
  }

  def toScalarTempValue(unboxedType : nfi.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = unboxedType match {
    case nfi.UnicodeChar =>
      toTempValue(vt.BoxedIntrinsicType(bt.BoxedCharacter)) map { boxedChar =>
        val unboxedTemp = new ps.TempValue
        plan.steps += ps.UnboxCharacter(unboxedTemp, boxedChar)

        unboxedTemp
      }
      
    case nfi.Utf8CString =>
      toTempValue(vt.BoxedIntrinsicType(bt.BoxedString)) map { boxedString =>
        val unboxedTemp = new ps.TempValue
        plan.steps += ps.UnboxStringAsUtf8(unboxedTemp, boxedString)

        unboxedTemp
      }

    case intType : nfi.IntType =>
      toTempValue(vt.BoxedIntrinsicType(bt.BoxedExactInteger)) map { boxedExactInt =>
        val unboxedTemp = new ps.TempValue
        plan.steps += ps.UnboxExactInteger(unboxedTemp, boxedExactInt)

        if (intType.bits == 64) {
          // Correct width
          unboxedTemp
        }
        else {
          val convTemp = new ps.TempValue

          // Convert to the right width
          plan.steps += ps.ConvertNativeInteger(convTemp, unboxedTemp, intType.bits, intType.signed) 
          convTemp
        }
      }

    case fpType : nfi.FpType =>
      val possiblyExactInt = possibleTypes.contains(bt.BoxedExactInteger)
      val possiblyInexactRational = possibleTypes.contains(bt.BoxedInexactRational)

      if (!possiblyExactInt && !possiblyInexactRational) {
        // Not possible
        None
      }
      else if (possiblyExactInt & !possiblyInexactRational) {
        toTempValue(vt.BoxedIntrinsicType(bt.BoxedExactInteger)) map { boxedExactInt =>
          // Unbox as exact int
          val unboxedTemp = new ps.TempValue
          plan.steps += ps.UnboxExactInteger(unboxedTemp, boxedExactInt)

          // Convert to the wanted type
          val convTemp = new ps.TempValue
          plan.steps += ps.ConvertNativeIntegerToFloat(convTemp, unboxedTemp, true, fpType)

          convTemp
        }
      }
      else if (!possiblyExactInt && possiblyInexactRational) {
        toTempValue(vt.BoxedIntrinsicType(bt.BoxedInexactRational)) map { boxedInexactRational =>
          // Unbox as inexact rational
          val unboxedTemp = new ps.TempValue
          plan.steps += ps.UnboxInexactRational(unboxedTemp, boxedInexactRational)

          if (fpType == nfi.Double) {
            // No conversion needed
            unboxedTemp
          }
          else {
            val convTemp = new ps.TempValue

            plan.steps += ps.ConvertNativeFloat(convTemp, unboxedTemp, fpType)
            convTemp
          }
        }
      }
      else {
        // We have to check types here and branch on the result
        val isExactIntPred = new ps.TempValue

        plan.steps += ps.TestBoxedType(isExactIntPred, tempValue, bt.BoxedExactInteger)

        // Try again with constrained types
        // This will hit the branches above us
        val trueWriter = plan.forkPlan()
        val trueDynamicValue = new BoxedIntrinsicValue(Set(bt.BoxedExactInteger), valueType, tempValue)
        val trueTempValue = trueDynamicValue.toRequiredTempValue(vt.ScalarType(fpType))(trueWriter)

        val falseWriter = plan.forkPlan()
        val falseDynamicValue = new BoxedIntrinsicValue(possibleTypes - bt.BoxedExactInteger, valueType, tempValue)
        val falseTempValue = falseDynamicValue.toRequiredTempValue(vt.ScalarType(fpType))(falseWriter)
      
        val phiTemp = new ps.TempValue

        plan.steps += ps.CondBranch(phiTemp, isExactIntPred, 
          trueWriter.steps.toList, trueTempValue,
          falseWriter.steps.toList, falseTempValue) 

        Some(phiTemp)
      }

    case _ =>
      None
  }
  
  protected def toBoxedRecordTempValue(recordType : vt.BoxedRecordType)(implicit plan : PlanWriter) : Option[ps.TempValue] = {
    // Convert ourselves to a record
    val recordTemp = toRequiredTempValue(vt.BoxedIntrinsicType(bt.BoxedRecord))

    // Make sure we we're of the right class
    plan.steps += ps.AssertBoxedRecordClass(tempValue, recordType)

    Some(recordTemp)
  }
}

