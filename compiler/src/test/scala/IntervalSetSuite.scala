package io.llambda.compiler
import io.llambda

import org.scalatest.FunSuite


class IntervalSetSuite extends FunSuite {
  test("empty range set contains no values") {
    val empty = IntervalSet()

    assertResult(true)  { empty.isEmpty }

    assertResult(false) { empty.contains(0L) }
    assertResult(false) { empty.contains(Long.MinValue) }
    assertResult(false) { empty.contains(Long.MaxValue) }

    assertResult(None) {
      empty.enclosingInterval
    }
  }

  test("single value range sets contain that value") {
    val singleValue = IntervalSet(42L)

    assertResult(false) { singleValue.isEmpty }

    assertResult(false) { singleValue.contains(41L) }
    assertResult(true)  { singleValue.contains(42L) }
    assertResult(false) { singleValue.contains(43L) }
    assertResult(false) { singleValue.contains(0L) }
    assertResult(false) { singleValue.contains(Long.MinValue) }
    assertResult(false) { singleValue.contains(Long.MaxValue) }

    assertResult(Some(Interval(42L, 42L))) {
      singleValue.enclosingInterval
    }
  }

  test("single interval range sets") {
    val singleInterval = IntervalSet(Interval(30L, 42L))

    assertResult(false) { singleInterval.isEmpty }


    assertResult(false) { singleInterval.contains(0L)  }
    assertResult(false) { singleInterval.contains(Long.MinValue) }
    assertResult(false) { singleInterval.contains(Long.MaxValue) }

    assertResult(Some(Interval(30L, 42L))) {
      singleInterval.enclosingInterval
    }
  }

  test("multi interval range sets") {
    val multiInterval = IntervalSet(Interval(-200L, -100L), Interval(30L, 42L))

    assertResult(false) { multiInterval.isEmpty }

    assertResult(false) { multiInterval.contains(-201L) }
    assertResult(true)  { multiInterval.contains(-200L) }
    assertResult(true)  { multiInterval.contains(-150L) }
    assertResult(true)  { multiInterval.contains(-100L) }
    assertResult(false) { multiInterval.contains(-99L) }

    assertResult(false) { multiInterval.contains(29L) }
    assertResult(true)  { multiInterval.contains(30L) }
    assertResult(true)  { multiInterval.contains(35L) }
    assertResult(true)  { multiInterval.contains(42L) }
    assertResult(false) { multiInterval.contains(43L) }

    assertResult(false) { multiInterval.contains(0L)  }
    assertResult(false) { multiInterval.contains(Long.MinValue) }
    assertResult(false) { multiInterval.contains(Long.MaxValue) }

    assertResult(Some(Interval(-200L, 42L))) {
      multiInterval.enclosingInterval
    }
  }

  test("interval set unions") {
    val empty = IntervalSet()

    val singleValue = empty ++ IntervalSet(42L)

    assertResult(false) { singleValue.contains(41L) }
    assertResult(true)  { singleValue.contains(42L) }
    assertResult(false) { singleValue.contains(43L) }

    val valueAndRange = IntervalSet(Interval(-42L, -30L)) ++ singleValue

    assertResult(false) { valueAndRange.contains(41L) }
    assertResult(true)  { valueAndRange.contains(42L) }
    assertResult(false) { valueAndRange.contains(43L) }

    assertResult(false) { valueAndRange.contains(-43L) }
    assertResult(true)  { valueAndRange.contains(-42L) }
    assertResult(true)  { valueAndRange.contains(-35L) }
    assertResult(true)  { valueAndRange.contains(-30L) }
    assertResult(false) { valueAndRange.contains(-29L) }

    val valueAndMultipleRange = IntervalSet(Interval(-200L, -100L)) ++ valueAndRange

    assertResult(false) { valueAndMultipleRange.contains(41L) }
    assertResult(true)  { valueAndMultipleRange.contains(42L) }
    assertResult(false) { valueAndMultipleRange.contains(43L) }

    assertResult(false) { valueAndMultipleRange.contains(-43L) }
    assertResult(true)  { valueAndMultipleRange.contains(-42L) }
    assertResult(true)  { valueAndMultipleRange.contains(-35L) }
    assertResult(true)  { valueAndMultipleRange.contains(-30L) }
    assertResult(false) { valueAndMultipleRange.contains(-29L) }

    assertResult(false) { valueAndMultipleRange.contains(-201L) }
    assertResult(true)  { valueAndMultipleRange.contains(-200L) }
    assertResult(true)  { valueAndMultipleRange.contains(-150L) }
    assertResult(true)  { valueAndMultipleRange.contains(-100L) }
    assertResult(false) { valueAndMultipleRange.contains(-99L) }

    assertResult(Some(Interval(-200L, 42L))) {
      valueAndMultipleRange.enclosingInterval
    }
  }
}
