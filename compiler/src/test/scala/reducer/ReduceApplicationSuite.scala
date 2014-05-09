package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class ReduceApplicationSuite extends FunSuite with Inside with testutil.ExpressionHelpers {
  // Use (scheme base) by default
  val schemeBaseBindings = libraryLoader.loadSchemeBase(frontendConfig)

  private def bindlessReductionFor(schemeString : String) = {
    implicit val freshScope = new Scope(collection.mutable.Map(schemeBaseBindings.toSeq : _*))

    // This removes any top-level Bind()s for lambdas
    // Just one pass of the reducer will leave the un-inlined versions of procedures behind. This is because it doesn't
    // know they're unused until all the callers are inlined. One solution would be to run the reducer twice but we 
    // want to ensure that the full inlining operation can happen in one pass.
    et.Expression.fromSequence(
      reductionFor(schemeString).toSequence.flatMap {
        case et.Bind(_) =>
          None

        case other =>
          Some(other)
      }
    )
  }

  test("inlining without arguments") {
    assert(bindlessReductionFor("""
      (define (trivial-return) 1)
      (trivial-return)
      """) === et.Literal(ast.IntegerLiteral(1))
    )
  }
  
  test("inlining one arg compile time evaluable") {
    assert(bindlessReductionFor("""
      (define (add-two n) (+ 2 n))
      (add-two 4)
      """) === et.Literal(ast.IntegerLiteral(6))
    )
  }
  
  test("inlining with two arguments") {
    assert(bindlessReductionFor("""
      (define (right-types should-bool should-null)
        (and (boolean? should-bool) (null? should-null)))
      (right-types #t '())
      """) === et.Literal(ast.BooleanLiteral(true))
    )
  }

  test("recursive inlining") {
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
