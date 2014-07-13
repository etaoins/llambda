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
  
  test("trivial inline of bound procedure") {
    assertStaticPlan("""
      (let ((return-15 (lambda () 15)))
        (return-15))
    """, ast.IntegerLiteral(15))
  }
  
  test("trivial inline of bound procedure with one argument") {
    assertStaticPlan("""
      (let ((return-arg (lambda (arg) arg)))
        (return-arg -10))
    """, ast.IntegerLiteral(-10))
  }
}
