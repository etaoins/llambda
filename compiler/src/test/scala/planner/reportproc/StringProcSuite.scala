package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class StringProcSuite extends FunSuite with PlanHelpers {
  test("(string-length)") {
    assertStaticPlan("""(string-length "")""",
      ast.IntegerLiteral(0)
    )

    assertStaticPlan("""(string-length "Hello!")""",
      ast.IntegerLiteral(6)
    )

    assertStaticPlan("""(string-length "Hell☃!")""",
      ast.IntegerLiteral(6)
    )

    assertStaticPlan("""(string-length "Hell🏂!")""",
      ast.IntegerLiteral(6)
    )
  }
}
