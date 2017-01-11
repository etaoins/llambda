package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.IntervalSet

trait KnownInteger extends IntermediateValue {
  val nativeType: vt.IntType

  /** Returns the possible values this integer can have */
  val possibleValues: IntervalSet
}
