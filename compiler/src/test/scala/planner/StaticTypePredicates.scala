package io.llambda.compiler.planner
import io.llambda

import llambda.compiler._
import org.scalatest.FunSuite

class StaticTypePredicateSuite extends FunSuite with PlanHelpers {
  test("null?") {
    assertStaticPlan("(null? '())", ast.BooleanLiteral(true))
    assertStaticPlan("(null? '(1 2 3))", ast.BooleanLiteral(false))
    assertStaticPlan("(null? #f)", ast.BooleanLiteral(false))
  }
}
