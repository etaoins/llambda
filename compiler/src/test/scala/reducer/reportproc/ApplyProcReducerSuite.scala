package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.FunSuite

class ApplyProcReducerSuite extends FunSuite with testutil.ExpressionHelpers {
  test("inlining without arguments using (apply)") {
    implicit val scope = schemeBaseScope

    assert(bindlessReductionFor("""
      (define (trivial-return) 1)
      (apply trivial-return '())
      """) === et.Literal(ast.IntegerLiteral(1))
    )
  }
 
  test("inlining report procedure using (apply)") {
    implicit val scope = schemeBaseScope

    assert(bindlessReductionFor("""
      (apply * '(2 3 10))
      """) === et.Literal(ast.IntegerLiteral(60))
    )
  }
  
  test("re-applying rest arg using (apply)") {
    implicit val scope = schemeBaseScope

    // This is intentionally similar to the basic structure of (case-lambda)
    assert(bindlessReductionFor("""
      ((lambda rest-args
        (apply (lambda (a b c)
          (+ a b c)) rest-args)
       ) 4 5 6)

      """) === et.Literal(ast.IntegerLiteral(15))
    )
  }
  
  test("selecting two inner lambdas based on rest arg length") {
    implicit val scope = schemeBaseScope

    // This is intentionally similar to the basic structure of (case-lambda)
    assert(bindlessReductionFor("""
      ((lambda rest-args
        (if (= (length rest-args) 3)
          (apply (lambda (a b c)
            (+ a b c)) rest-args)
          -5)  
       ) 4 5 6)

      """) === et.Literal(ast.IntegerLiteral(15))
    )
  }
}
