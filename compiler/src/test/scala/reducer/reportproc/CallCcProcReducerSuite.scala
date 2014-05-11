package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class CallCcProcReducerSuite extends FunSuite with Inside with testutil.ExpressionHelpers {
  implicit val scope = schemeBaseScope
  
  test("trivial (call/cc)") {
    // This can be turned in to a literal value
    assert(reductionFor("(call/cc (lambda (return) (return 5) 'unused))") ===
      et.Literal(ast.IntegerLiteral(5))
    )
  }
  
  test("nested trivial (call/cc)") {
    // This tests both (call/cc) not using its arg and nesting
    // This can be reduced to a constant value also
    assert(reductionFor("""
        (call/cc (lambda (outer-return)
          (call/cc (lambda (inner-return)
            (inner-return 12)))))""") ===
      et.Literal(ast.IntegerLiteral(12))
    )
  }
  
  test("(call/cc) with multiple returns") {
    // This should be turned in to a normal lambda without call/cc
    inside(reductionFor("""
        (call/cc (lambda (return)
          (if (car (cons 1 2))
            (return 5)
            (return 6)
        )))""")) {
      case et.Apply(
        et.Lambda(List(), None,
          et.Cond(_,
            et.Return(et.Literal(ast.IntegerLiteral(5))),
            et.Return(et.Literal(ast.IntegerLiteral(6)))
          )
        ), List()
      ) =>
        Unit
    }
  }
}
