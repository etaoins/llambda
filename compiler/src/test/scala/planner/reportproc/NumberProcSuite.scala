package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class NumberProcSuite extends FunSuite with PlanHelpers {
  test("static (+)") {
    assertStaticPlan("(+)",
      ast.IntegerLiteral(0)
    )
    
    assertStaticPlan("(+ 56)",
      ast.IntegerLiteral(56)
    )
    
    assertStaticPlan("(+ 56 -6)",
      ast.IntegerLiteral(50)
    )
    
    assertStaticPlan("(+ 56 (+ -3 -3))",
      ast.IntegerLiteral(50)
    )
  }
  
  test("static (-)") {
    assertStaticPlan("(- 5)",
      ast.IntegerLiteral(-5)
    )
    
    assertStaticPlan("(- 200 50)",
      ast.IntegerLiteral(150)
    )
  }
  
  test("static (*)") {
    assertStaticPlan("(*)",
      ast.IntegerLiteral(1)
    )
    
    assertStaticPlan("(* 56)",
      ast.IntegerLiteral(56)
    )
    
    assertStaticPlan("(* 5 -6)",
      ast.IntegerLiteral(-30)
    )
  }
}
