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

  test("static (=)") {
    assertStaticPlan("(= 50 50)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(= 50 200)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(= 200 50)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(= 50.0 50.0)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(= 50.0 200.0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(= 200.0 50.0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(= 50 50.0)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(= 50.0 50)",
      ast.BooleanLiteral(true)
    )
  }
  
  test("static (<)") {
    assertStaticPlan("(< 50 50)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(< 50 200)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(< 200 50)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(< 50.0 50.0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(< 50.0 200.0)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(< 200.0 50.0)",
      ast.BooleanLiteral(false)
    )
  }
  
  test("static (<=)") {
    assertStaticPlan("(<= 50 50)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(<= 50 200)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(<= 200 50)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(<= 50.0 50.0)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(<= 50.0 200.0)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(<= 200.0 50.0)",
      ast.BooleanLiteral(false)
    )
  }
  
  test("static (>)") {
    assertStaticPlan("(> 50 50)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(> 50 200)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(> 200 50)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(> 50.0 50.0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(> 50.0 200.0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(> 200.0 50.0)",
      ast.BooleanLiteral(true)
    )
  }
  
  test("static (>=)") {
    assertStaticPlan("(>= 50 50)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(>= 50 200)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(>= 200 50)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(>= 50.0 50.0)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(>= 50.0 200.0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(>= 200.0 50.0)",
      ast.BooleanLiteral(true)
    )
  }
  
  test("static (exact)") {
    assertStaticPlan("(exact 50)",
      ast.IntegerLiteral(50)
    )
    
    assertStaticPlan("(exact -60.0)",
      ast.IntegerLiteral(-60)
    )
  }
  
  test("static (inexact)") {
    assertStaticPlan("(inexact 50)",
      ast.FlonumLiteral(50.0)
    )
    
    assertStaticPlan("(inexact -60.0)",
      ast.FlonumLiteral(-60.0)
    )
  }
  
  test("static (positive?)") {
    assertStaticPlan("(positive? 50)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(positive? 0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(positive? -200.0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(positive? +nan.0)",
      ast.BooleanLiteral(false)
    )
  }
  
  test("static (negative?)") {
    assertStaticPlan("(negative? 50)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(negative? 0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(negative? -200.0)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(negative? +nan.0)",
      ast.BooleanLiteral(false)
    )
  }
  
  test("static (zero?)") {
    assertStaticPlan("(zero? 50)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(zero? 0)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(zero? -200.0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(zero? +nan.0)",
      ast.BooleanLiteral(false)
    )
  }

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
