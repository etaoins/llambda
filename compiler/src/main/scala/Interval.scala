package io.llambda.compiler


/** Class representing an interval of Long values
  *
  * This is similar to a Scala Range without the concept of a step
  */
case class Interval(start: Long, end: Long) {
  def contains(value: Long): Boolean =
    (this.start <= value) && (this.end >= value)

  def subsetOf(other: Interval): Boolean =
    (other.start <= this.start) && (other.end >= this.end)
}
