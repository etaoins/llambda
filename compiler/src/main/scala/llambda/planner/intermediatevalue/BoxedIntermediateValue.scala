package llambda.planner.intermediatevalue

import llambda.planner.{step => ps}
import llambda.planner.PlanWriter
import llambda.{boxedtype => bt}

trait BoxedIntermediateValue extends IntermediateValue {
  val valueType : bt.BoxedType   
  val tempValue : ps.TempValue

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

}

