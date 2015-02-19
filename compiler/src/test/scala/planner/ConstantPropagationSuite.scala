package io.llambda.compiler.planner
import io.llambda

import llambda.compiler._
import org.scalatest.FunSuite

class ConstantPropagationSuite extends FunSuite with PlanHelpers {
  test("value propagated through variable") {
    assertStaticPlan("""
      (define x (+ 2 3))
      x
    """, ast.IntegerLiteral(5))
  }

  test("value propagated through (cons)") {
    assertStaticPlan("""
      (define x (cons 2 3))
      (+ (car x) (cdr x))
    """, ast.IntegerLiteral(5))
  }

  test("value propagated through (values)") {
    assertStaticPlan("""
      (let-values (((x y) (values 2 3)))
        (+ x y))
    """, ast.IntegerLiteral(5))
  }

  test("value propagated through (list)") {
    assertStaticPlan("""
      (define test-list '(1 2 3 4))
      (+ (list-ref test-list 1) (list-ref test-list 2))
    """, ast.IntegerLiteral(5))
  }

  test("value propagated through immutable record types") {
    assertStaticPlan("""
      (define-record-type <int-pair> (int-pair left right) int-pair?
        (left int-pair-left)
        (right int-pair-right))

      (define test-int-pair (int-pair 2 3))
      (+ (int-pair-left test-int-pair) (int-pair-right test-int-pair))
    """, ast.IntegerLiteral(5))
  }
}
