package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class InexactProcSuite extends FunSuite with PlanHelpers {
  test("static (nan?)") {
    assertStaticPlan("(nan? 50)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(nan? 55.5)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(nan? +nan.0)",
      ast.BooleanLiteral(true)
    )
  }
}
