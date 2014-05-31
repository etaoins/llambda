package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class ProcHasSideEffectsSuite extends FunSuite with Inside with testutil.ExpressionHelpers {
  test("(list) does not have side effects") {
    implicit val scope = schemeBaseScope

    assert(reductionFor("""
      (list 1 2 3)
      4
      """) === et.Literal(ast.IntegerLiteral(4))
    )
  }
  
  test("(vector) does not have side effects") {
    implicit val scope = schemeBaseScope

    assert(reductionFor("""
      (vector 1 2 3)
      4
      """) === et.Literal(ast.IntegerLiteral(4))
    )
  }
  
  test("(vector) with side effecting argument has side effects") {
    implicit val scope = schemeBaseScope

    // XXX: We should be able to strip the (vector) away completely here
    inside(reductionFor("""(vector 1 (set-cdr! vector 1) 3) 4""")) {
      case et.Begin(List(et.Apply(_, _), et.Literal(ast.IntegerLiteral(4)))) =>
        Unit
    }
  }
  
  test("(input-port?) does not have side effects with correct arity") {
    implicit val scope = schemeBaseScope

    assert(reductionFor("""
      (input-port? #t)
      4
      """) === et.Literal(ast.IntegerLiteral(4))
    )
  }
  
  test("(input-port?) does has side effects with incorrect arity") {
    implicit val scope = schemeBaseScope

    inside(reductionFor("""(input-port? #t #t) 4""")) {
      case et.Begin(List(et.Apply(_, _), et.Literal(ast.IntegerLiteral(4)))) =>
        Unit
    }
  }
}
