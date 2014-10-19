package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class CharProcSuite extends FunSuite with PlanHelpers {
  test("(char->integer)") {
    assertStaticPlan("""(char->integer #\A)""",
      ast.IntegerLiteral(65)
    )
  }

  test("(integer->)") {
    assertStaticPlan("""(integer->char 65)""",
      ast.CharLiteral('A')
    )
  }
}
