package llambda.planner.intermediatevalue

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{PlanWriter, InvokableProcedure}

class DynamicBoxedValue(val possibleTypes : Set[bt.ConcreteBoxedType], valueType : bt.BoxedType, tempValue : ps.TempValue) extends IntermediateValue {
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
  
  def toInvokableProcedure()(implicit plan : PlanWriter) : Option[InvokableProcedure] = 
    // XXX: Unboxing procedures
    None

  def toBoxedTempValue(targetType : bt.BoxedType)(implicit plan : PlanWriter) : Option[ps.TempValue] = {
    val targetConcreteTypes = targetType.concreteTypes

    // Are our possible concrete types a subset of the target types?
    if (possibleTypes.subsetOf(targetConcreteTypes)) {
      if (valueType != targetType) {
        // Need to cast to the right type
        // We've confirmed that no checking is needed because all of our 
        // possible types are equal to or supertypes of the target type
        val castTemp = new ps.TempValue
        plan.steps += ps.CastBoxedToTypeUnchecked(castTemp, tempValue, targetType)

        Some(castTemp)
      }
      else {
        // We're already of the required type
        Some(tempValue)
      }
    }
    else if (!possibleTypes.intersect(targetConcreteTypes).isEmpty) {
      val castTemp = new ps.TempValue
      plan.steps += ps.CastBoxedToSubtypeChecked(castTemp, tempValue, targetType)
      Some(castTemp)
    }
    else {
      // Not possible
      None
    }
  }

  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit plan : PlanWriter) : Option[ps.TempValue] = unboxedType match {
    case nfi.CStrictBool =>
      // Make sure we're actually a boolean
      toTempValue(nfi.BoxedValue(bt.BoxedBoolean)) map { _ =>
        toTempValue(nfi.CTruthyBool).get
      }

    case nfi.UnicodeChar =>
      toTempValue(nfi.BoxedValue(bt.BoxedCharacter)) map { boxedChar =>
        val unboxedTemp = new ps.TempValue
        plan.steps += ps.UnboxCharacter(unboxedTemp, boxedChar)

        unboxedTemp
      }
      
    case nfi.Utf8CString =>
      toTempValue(nfi.BoxedValue(bt.BoxedString)) map { boxedString =>
        val unboxedTemp = new ps.TempValue
        plan.steps += ps.UnboxStringAsUtf8(unboxedTemp, boxedString)

        unboxedTemp
      }

    case intType : nfi.IntType =>
      toTempValue(nfi.BoxedValue(bt.BoxedExactInteger)) map { boxedExactInt =>
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
        toTempValue(nfi.BoxedValue(bt.BoxedExactInteger)) map { boxedExactInt =>
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
        toTempValue(nfi.BoxedValue(bt.BoxedInexactRational)) map { boxedInexactRational =>
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
        val trueDynamicValue = new DynamicBoxedValue(Set(bt.BoxedExactInteger), valueType, tempValue)
        val trueTempValue = trueDynamicValue.toRequiredTempValue(fpType)(trueWriter)

        val falseWriter = plan.forkPlan()
        val falseDynamicValue = new DynamicBoxedValue(possibleTypes - bt.BoxedExactInteger, valueType, tempValue)
        val falseTempValue = falseDynamicValue.toRequiredTempValue(fpType)(falseWriter)
      
        val phiTemp = new ps.TempValue

        plan.steps += ps.CondBranch(phiTemp, isExactIntPred, 
          trueWriter.steps.toList, trueTempValue,
          falseWriter.steps.toList, falseTempValue) 

        Some(phiTemp)
      }

    case _ =>
      None
  }
}

