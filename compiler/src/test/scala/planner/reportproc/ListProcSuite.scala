package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class ListProcSuite extends FunSuite with PlanHelpers {
  test("static (length)") {
    assertStaticPlan("(length '())",
      ast.IntegerLiteral(0)
    )
    
    assertStaticPlan("(length '(1))",
      ast.IntegerLiteral(1)
    )
    
    assertStaticPlan("(length '(1 2 3))",
      ast.IntegerLiteral(3)
    )
  }
}
