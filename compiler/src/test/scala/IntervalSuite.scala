package io.llambda.compiler
import io.llambda

import org.scalatest.FunSuite


class IntervalSuite extends FunSuite {
  test("subsetOf") {
    val fortyTwoInterval = Interval(42L, 42L)
    val fortiesInterval = Interval(40L, 49L)
    val firstHundredInterval = Interval(0L, 100L)

    assertResult(true)  { fortyTwoInterval.subsetOf(fortiesInterval) }
    assertResult(true)  { fortyTwoInterval.subsetOf(firstHundredInterval) }
    assertResult(true)  { fortyTwoInterval.subsetOf(fortyTwoInterval) }

    assertResult(false) { fortiesInterval.subsetOf(fortyTwoInterval) }
    assertResult(true)  { fortiesInterval.subsetOf(fortiesInterval) }
    assertResult(true)  { fortiesInterval.subsetOf(firstHundredInterval) }

    assertResult(false) { firstHundredInterval.subsetOf(fortyTwoInterval) }
    assertResult(false) { firstHundredInterval.subsetOf(fortiesInterval) }
    assertResult(true)  { firstHundredInterval.subsetOf(firstHundredInterval) }
  }

}
