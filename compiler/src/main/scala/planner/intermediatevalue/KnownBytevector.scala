package io.llambda.compiler.planner.intermediatevalue


/** Value of a bytevector with a known length
  *
  * This allows inlining (bytevector-u8-ref) and (bytevector-u8-set!) for known bytevectors
  */
trait KnownBytevector extends IntermediateValue {
  /** Length of the bytevector */
  val bytevectorLength: Long
}
