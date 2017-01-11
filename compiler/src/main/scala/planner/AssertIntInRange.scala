package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{RuntimeErrorMessage, ErrorCategory, IntervalSet}

object AssertIntInRange {
  /** Plans a runtime assertion that a native integer value can fit within the given target integer type
    *
    * If all possible values of the source integer can be represented by the target type then no assertions will be
    * generated.
    *
    * @param  tempValue    Value of fromType to be converted to the target type
    * @param  fromType     Type of the tempValue
    * @param  toType       Target integer type
    * @param  evidenceOpt  Datum cell evidence for the signalled error
    */
  def apply(
      tempValue: ps.TempValue,
      fromType: vt.IntType,
      toType: vt.IntType,
      evidenceOpt: Option[ps.TempValue] = None
  )
  (possibleValues: IntervalSet = IntervalSet(fromType.valueInterval))
  (implicit plan: PlanWriter): Unit = {
    val enclosingValueInterval = possibleValues.enclosingInterval.get

    if (enclosingValueInterval.start < toType.minIntValue) {
      val lowerRangeTemp = ps.Temp(toType)
      plan.steps += ps.CreateNativeInteger(lowerRangeTemp, toType.minIntValue, fromType.bits)

      val withinLowerRangeTemp = ps.Temp(vt.Predicate)
      plan.steps += ps.IntegerCompare(
        result=withinLowerRangeTemp,
        cond=ps.CompareCond.GreaterThanEqual,
        signed=Some(fromType.signed),
        val1=tempValue,
        val2=lowerRangeTemp
      )

      val errorMessage = RuntimeErrorMessage(
        category=ErrorCategory.Type,
        name=s"intTooSmallFor${toType}",
        text=s"Integer value too small to be represented by native integer type ${toType}"
      )

      plan.steps += ps.AssertPredicate(withinLowerRangeTemp, errorMessage, evidenceOpt)
    }

    if (enclosingValueInterval.end > toType.maxIntValue) {
      val upperRangeTemp = ps.Temp(toType)
      plan.steps += ps.CreateNativeInteger(upperRangeTemp, toType.maxIntValue, fromType.bits)

      val withinUpperRangeTemp = ps.Temp(vt.Predicate)
      plan.steps += ps.IntegerCompare(
        result=withinUpperRangeTemp,
        cond=ps.CompareCond.LessThanEqual,
        signed=Some(fromType.signed),
        val1=tempValue,
        val2=upperRangeTemp
      )

      val errorMessage = RuntimeErrorMessage(
        category=ErrorCategory.Type,
        name=s"intTooLargeFor${toType}",
        text=s"Integer value too large to be represented by native integer type ${toType}"
      )

      plan.steps += ps.AssertPredicate(withinUpperRangeTemp, errorMessage, evidenceOpt)
    }
  }
}
