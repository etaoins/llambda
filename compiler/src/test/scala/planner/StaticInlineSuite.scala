package io.llambda.compiler.planner
import io.llambda

import llambda.compiler._
import org.scalatest.FunSuite

class StaticInlineSuite extends FunSuite with PlanHelpers {
  test("trivial self-executing inline") {
    assertStaticPlan("""
      ((lambda () 15))
    """, ast.IntegerLiteral(15))
  }
  
  test("self-executing inline with one argument") {
    assertStaticPlan("""
      ((lambda (arg) arg) 25)
    """, ast.IntegerLiteral(25))
  }
}
