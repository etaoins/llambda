package io.llambda.compiler.planner
import io.llambda

import llambda.compiler._
import org.scalatest.FunSuite

class StaticBranchSuite extends FunSuite with PlanHelpers {
  test("trivial branch on boolean") {
    assertStaticPlan("""(if #f 1 2)""", ast.IntegerLiteral(2))
    assertStaticPlan("""(if #t 1 2)""", ast.IntegerLiteral(1))
  }
  
  test("trivial branch on non-boolean") {
    assertStaticPlan("""(if 1 1 2)""", ast.IntegerLiteral(1))
  }

  test("nested branches") {
    assertStaticPlan("""(if (if #t #f #t) 1 2)""", ast.IntegerLiteral(2))
  }
  
  test("negated branch condition") {
    assertStaticPlan("""(if (not #f) 1 2)""", ast.IntegerLiteral(1))
  }
}
