package io.llambda.compiler
import io.llambda


/** Class representing a set of Intervals
  *
  * This is an extremely naive implementation. If many distinct intervals are required this class will have poor
  * performance characteristics.
  */
class IntervalSet private (private val intervals: Set[Interval]) {
  def contains(value: Long): Boolean =
    intervals.exists(_.contains(value))

  def isEmpty = intervals.isEmpty

  def enclosingInterval: Option[Interval] = if (isEmpty) {
    None
  }
  else {
    val minStart = intervals.map(_.start).min
    val maxEnd = intervals.map(_.end).max

    Some(Interval(minStart, maxEnd))
  }

  def ++(other: IntervalSet): IntervalSet =
    new IntervalSet(this.intervals ++ other.intervals)
}

object IntervalSet {
  def apply(): IntervalSet =
    new IntervalSet(Set())

  def apply(value: Long): IntervalSet =
    new IntervalSet(Set(Interval(value, value)))

  def apply(intervals: Interval*): IntervalSet = {
    new IntervalSet(Set(intervals: _*))
  }
}
