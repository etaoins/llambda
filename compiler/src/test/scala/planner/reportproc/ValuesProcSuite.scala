package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class ValuesProcSuite extends FunSuite with PlanHelpers {
  test("static (call-with-values)") {
    assertStaticPlan("""
      (call-with-values
        (lambda () (values 2 3 4))
        (lambda (a b c) (+ a b c)))
      """, ast.IntegerLiteral(9)
    )
  }
}
