package io.llambda.compiler.planner
import io.llambda

import llambda.compiler._
import org.scalatest.FunSuite

class StaticBranchSuite extends FunSuite with PlanHelpers {
  test("trivial branch on boolean") {
    assertStaticPlan("(if #f 'true 'false)",
      ast.Symbol("false")
    )

    assertStaticPlan("(if #t 'true 'false)",
      ast.Symbol("true")
    )
  }
  
  test("trivial branch on non-boolean") {
    assertStaticPlan("(if 1 'true 'false)",
      ast.Symbol("true")
    )
  }

  test("nested branches") {
    assertStaticPlan("(if (if #t #f #t) 'true 'false)",
      ast.Symbol("false")
    )
  }
  
  test("branch conditions on boolean operations") {
    assertStaticPlan("(if (not #f) 'true 'false)",
      ast.Symbol("true")
    )
    
    assertStaticPlan("(if (or #f #t) 'true 'false)",
      ast.Symbol("true")
    )
    
    assertStaticPlan("(if (or #f #f) 'true 'false)",
      ast.Symbol("false")
    )
    
    assertStaticPlan("(if (and #t #t) 'true 'false)",
      ast.Symbol("true")
    )
    
    assertStaticPlan("(if (and #t #f) 'true 'false)",
      ast.Symbol("false")
    )
  }
}
