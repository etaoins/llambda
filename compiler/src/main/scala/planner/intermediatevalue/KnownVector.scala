package io.llambda.compiler.planner.intermediatevalue
import io.llambda

/** Value of a vector with a known length
  *
  * This allows inlining (vector-ref) and (vector-set!) for known vectors
  */
trait KnownVector extends IntermediateValue {
  /** Length of the vector */
  val vectorLength: Long
}
