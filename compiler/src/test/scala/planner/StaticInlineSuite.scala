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
  
  test("self-executing inline with only empty rest argument") {
    assertStaticPlan("""
      ((lambda rest-args (null? rest-args)))
    """, ast.BooleanLiteral(true))
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
 
  test("trivial inline of bound procedure with one argument and empty rest arg") {
    assertStaticPlan("""
      (let ((has-no-args? (lambda rest-args (null? rest-args))))
        (has-no-args?))
    """, ast.BooleanLiteral(true))
  }

  test("inlining procedures passed as arguments") {
    assertStaticPlan("""
      (define (add2 val) (+ val 2))
      (define (apply-proc-to-arg proc arg)
        (proc arg))
      (apply-proc-to-arg add2 6)
    """, ast.IntegerLiteral(8))
  }

  test("inlining procedures returned from other exprs") {
    assertStaticPlan("""
      (define proc-from-let (let ((mutliplier 2))
        (lambda (to-multiply)
          (* mutliplier to-multiply))))
      (proc-from-let -5)
    """, ast.IntegerLiteral(-10))
  }

  test("reducing procedures decided by a conditional") {
    assertStaticPlan("""
      (define (multi-op val1 val2 use-minus)
        (define operator (if use-minus - +))
        (operator val1 val2))
      (multi-op 4 10 #f)
    """, ast.IntegerLiteral(14))
  }

  test("recursive inlining") {
    assertStaticPlan("""
      (define (add-two n)
        (+ 2 n))
      (define (times-four n)
        (* (add-two 2) n))
      (times-four 8)
    """, ast.IntegerLiteral(32))
  }
}
