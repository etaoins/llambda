package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.OutOfBoundsException
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class VectorSuite extends FunSuite with PlanHelpers {
  test("type predicates") {
    assertStaticPlan("(vector? #(0 1 2))",
     ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(vector? #f)",
     ast.BooleanLiteral(false)
    )
  }
  
  test("static (vector-ref)") {
    assertStaticPlan("(vector-ref #(1 2 3 4 5) 3)",
      ast.IntegerLiteral(4)
    )

    intercept[OutOfBoundsException] {
      assertStaticPlan("(vector-ref #(1 2 3 4 5) 6)",
        ast.IntegerLiteral(4)
      )
    }
    
    intercept[OutOfBoundsException] {
      assertStaticPlan("(vector-ref #(1 2 3 4 5) -5)",
        ast.IntegerLiteral(4)
      )
    }
  }

  test("static (vector-length)") {
    assertStaticPlan("(vector-length #(1 2 3 4 5))",
      ast.IntegerLiteral(5)
    )

    assertStaticPlan("(vector-length #())",
      ast.IntegerLiteral(0)
    )
  }
}
