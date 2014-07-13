package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class CallCcProcSuite extends FunSuite with PlanHelpers {
  test("trivial (call/cc)") {
    assertStaticPlan("(call/cc (lambda (return) (return 5) 'unused))",
      ast.IntegerLiteral(5)
    )
  }

  test("nested trivial (call/cc)") {
    assertStaticPlan("""
      (call/cc (lambda (outer-return)
        (call/cc (lambda (inner-return)
          (inner-return 12)))))
      """, ast.IntegerLiteral(12)
    )
  }
}
