package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.OutOfBoundsException
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class BytevectorSuite extends FunSuite with PlanHelpers {
  test("static (bytevector-length)") {
    assertStaticPlan("(bytevector-length #u8(1 2 3 4 5))",
      ast.IntegerLiteral(5)
    )

    assertStaticPlan("(bytevector-length #u8())",
      ast.IntegerLiteral(0)
    )
  }
}
