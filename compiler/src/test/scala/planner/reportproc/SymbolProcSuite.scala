package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class SymbolProcSuite extends FunSuite with PlanHelpers {
  test("(string->symbol)") {
    assertStaticPlan("""(string->symbol "Hello, world!")""",
      ast.Symbol("Hello, world!")
    )
  }

  test("(symbol->string)") {
    assertStaticPlan("""(symbol->string '|Hello, world!|)""",
      ast.StringLiteral("Hello, world!")
    )
  }
}
