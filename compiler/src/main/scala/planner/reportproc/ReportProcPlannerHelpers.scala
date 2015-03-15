package io.llambda.compiler.planner.reportproc
import io.llambda

import io.llambda.compiler.RangeException
import io.llambda.compiler.planner._
import io.llambda.compiler.{valuetype => vt}

trait ReportProcPlannerHelpers {
  /** Asserts that an index is within an object of the passed length
    *
    * If this fails a RangeException will be thrown; the function will return normally otherwise. This will throw
    * errors consistent with runtime/util/rangeAssertions.h
    */
  protected def assertIndexValid(procName : String, length : Long, index : Long)(implicit plan : PlanWriter) : Unit =
    if (index >= length) {
      throw new RangeException(
        plan.activeContextLocated,
        s"Index of ${index} is past length of ${length} in ${procName}"
      )
    }
    else if (index < 0) {
      throw new RangeException(
        plan.activeContextLocated,
        s"Negative index of ${index} in ${procName}"
      )
    }

  /** Asserts that a length is valid for a given object type
    *
    * If this fails a RangeException will be thrown; the function will return normally otherwise.
    */
  protected def assertLengthValid(
      procName : String,
      lengthName : String,
      maxLength : Long,
      length : Long
  )(implicit plan : PlanWriter) : Unit =
    if (length > maxLength) {
      throw new RangeException(
        plan.activeContextLocated,
        s"Length of ${length} exceeds maximum ${lengthName} of ${maxLength} in ${procName}"
      )
    }
    else if (length < 0) {
      throw new RangeException(
        plan.activeContextLocated,
        s"Negative ${lengthName} of ${length} in ${procName}"
      )
    }
}
