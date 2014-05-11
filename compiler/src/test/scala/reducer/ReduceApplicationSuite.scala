package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class ReduceApplicationSuite extends FunSuite with Inside with testutil.ExpressionHelpers { 
  test("inlining without arguments") {
    implicit val scope = schemeBaseScope

    assert(bindlessReductionFor("""
      (define (trivial-return) 1)
      (trivial-return)
      """) === et.Literal(ast.IntegerLiteral(1))
    )
  }
   
  test("inlining one arg compile time evaluable") {
    implicit val scope = schemeBaseScope

    assert(bindlessReductionFor("""
      (define (add-two n) (+ 2 n))
      (add-two 4)
      """) === et.Literal(ast.IntegerLiteral(6))
    )
  }
  
  test("inlining with empty rest arguments") {
    implicit val scope = schemeBaseScope

    assert(bindlessReductionFor("""
      (define (all-rest . rest-arg) (null? rest-arg))
      (all-rest)
      """) === et.Literal(ast.BooleanLiteral(true))
    )
  }
  
  test("counting the number of rest arguments") {
    implicit val scope = schemeBaseScope

    assert(bindlessReductionFor("""
      (define (length-rest first . rest) (+ first (length rest)))
      (length-rest 4 2 3 4 5)
      """) === et.Literal(ast.IntegerLiteral(8))
    )
  }
  
  test("inlining with two arguments") {
    implicit val scope = schemeBaseScope

    assert(bindlessReductionFor("""
      (define (right-types should-bool should-null)
        (and (boolean? should-bool) (null? should-null)))
      (right-types #t '())
      """) === et.Literal(ast.BooleanLiteral(true))
    )
  }

  test("recursive inlining") {
    implicit val scope = schemeBaseScope

    assert(bindlessReductionFor("""
      (define (add-two n)
        (+ 2 n))
      (define (times-four n)
        (* (add-two 2) n))
      (times-four 8)
      """) === et.Literal(ast.IntegerLiteral(32))
    )
  }
}
