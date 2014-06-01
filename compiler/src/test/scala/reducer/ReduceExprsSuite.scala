package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class ReduceExprsSuite extends FunSuite with Inside with testutil.ExprHelpers {
  // Use (scheme base) by default
  implicit val scope = schemeBaseScope

  test("reducing basic constants") {
    assert(reductionFor("'test-symbol") === 
      et.Literal(ast.Symbol("test-symbol"))
    )
  }
  
  test("collapsing static branches") {
    assert(reductionFor("(if #t 'true 'false)") ===
      et.Literal(ast.Symbol("true"))
    )
    
    assert(reductionFor("(if #f 'true 'false)") === 
      et.Literal(ast.Symbol("false"))
    )
    
    assert(reductionFor("(if (and #t #f) 'true 'false)") === 
      et.Literal(ast.Symbol("false"))
    )
    
    assert(reductionFor("(if (or #f #t) 'true 'false)") === 
      et.Literal(ast.Symbol("true"))
    )
    
    assert(reductionFor("(if (or #f #f) 'true 'false)") === 
      et.Literal(ast.Symbol("false"))
    )
  }

  test("impure branch conditions are preserved") {
    inside(reductionFor("(if (begin (raise 'error) #t) 'true 'false)")) {
      case et.Begin(List(_ : et.Apply, et.Literal(ast.Symbol("true")))) =>
        Unit
    }
  }
  
  test("unused pure values") {
    assert(reductionFor("(begin 20.0 not 'symbol 10)") ===
      et.Literal(ast.IntegerLiteral(10))
    )
  }

  test("unused pure bindings") {
    assert(reductionFor("(define unused 1)") ===
      et.Begin(Nil)
    )
  }

  test("eventual termination while resolving recursive variables") {
    // This is an invalid Scheme program so it doesn't matter what we reduce to
    // The important part is we don't get stuck in a loop trying to reduce
    reductionFor("""
      (letrec ((x y) (y x))
        (if x
          'true
          'false))
      """)
  }

  test("complex nested collapsing") {
    assert(reductionFor("""
      (if (symbol=? 'symbol 'symbol)
        (* (string-length "Hello") (+ 1 (length '(1 2 3 4))))
        (boolean=? #t #f))
      """) === et.Literal(ast.IntegerLiteral(25))
    )
  }

  test("reducing (case)") {
    assert(reductionFor("""
      (case 'a
        ((a e i o u) 'vowel)
        ((w y) 'semivowel)
        (else => (lambda (x) x)))
      """) === et.Literal(ast.Symbol("vowel"))
    )
    
    assert(reductionFor("""
      (case 'c
        ((a e i o u) 'vowel)
        ((w y) 'semivowel)
        (else => (lambda (x) x)))
      """) === et.Literal(ast.Symbol("c"))
    )
  }

  test("reducing (case-lambda)") {
    val caseLambdaName = List("scheme", "case-lambda").map(StringComponent(_))
    val caseLambdaBindings = libraryLoader.load(caseLambdaName)(frontendConfig)
    val allBindings = schemeBaseBindings.toSeq ++ caseLambdaBindings.toSeq

    val testScope = new Scope(collection.mutable.Map(allBindings : _*))

    assert(reductionFor("""
      (define case-function
        (case-lambda
          ((first) (- first))
          ((first second) (* first second))))

      (+ (case-function 4 5) (case-function 1))
      """)(testScope) === et.Literal(ast.IntegerLiteral(19))
    )
  }
}
