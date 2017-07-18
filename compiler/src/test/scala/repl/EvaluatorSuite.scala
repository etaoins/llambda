package io.llambda.compiler.repl
import io.llambda

import io.llambda.compiler._

import org.scalatest.FunSuite
import llambda.compiler.SchemeStringImplicits._

class EvaluatorSuite extends FunSuite {
  private val targetPlatform = platform.DetectLlvmTarget()

  private def testEvaluator(): Evaluator =
    new Evaluator(targetPlatform)

  test("evaluating a trivial expression") {
    val eval = testEvaluator()

    assert(eval(datum"""(+ 1 2)""") === "3")
  }

  test("simple (define)") {
    val eval = testEvaluator()

    assert(eval(datum"""(define x 2)""") === "x => 2")
    assert(eval(datum"""2""") === "2")
  }

  test("(define) is identified by binding") {
    val eval = testEvaluator()

    // Try renaming (define)
    assert(eval(datum"""(import (rename (llambda base) (define my-define)))""") === "loaded")

    assert(eval(datum"""(my-define x 2)""") === "x => 2")
    assert(eval(datum"""2""") === "2")
  }

  test("typed (define)") {
    val eval = testEvaluator()

    assert(eval(datum"""(import (llambda typed))""") === "loaded")
    assert(eval(datum"""(define x : <integer> 10)""") === "x => 10")
    assert(eval(datum"""x""") === "10")
  }

  test("lambda shorthand (define)") {
    val eval = testEvaluator()

    eval(datum"""(define (return-first x y) x)""")
    assert(eval(datum"""(return-first 10 20)""") === "10")
  }

  test("(define) memoizes Scheme data") {
    val eval = testEvaluator()

    assert(eval(datum"""(import (llambda time))""") === "loaded")

    eval(datum"""(define captured-time (current-jiffy))""")

    // Make sure we're not evaluating (current-jiffy) multiple times
    val capturedTime1 = eval(datum"""captured-time""")
    val capturedTime2 = eval(datum"""captured-time""")

    assert(capturedTime1 === capturedTime2)
  }

  test("failed (define) doesnt leave binding") {
    val eval = testEvaluator()

    assert(eval(datum"""(import (llambda typed))""") === "loaded")
    assert(eval(datum"""(import (llambda read))""") === "loaded")

    intercept[ReplProcessNonZeroExitException] {
      // 1.5 isn't an integer
      eval(datum"""(define x : <integer> (string->number "1.5"))""")
    }

    intercept[UnboundVariableException] {
      // Make sure x isn't bound to anything
      assert(eval(datum"""x""") === "10")
    }
  }

  test("simple (set!)") {
    val eval = testEvaluator()

    assert(eval(datum"""(import (llambda time))""") === "loaded")

    // Define (captured-time) to one value
    eval(datum"""(define captured-time (current-jiffy))""")
    val capturedTime1 = eval(datum"""captured-time""")

    // Set it to another value
    eval(datum"""(set! captured-time (current-jiffy))""")
    val capturedTime2 = eval(datum"""captured-time""")

    // Make sure they're distinct
    assert(capturedTime1 !== capturedTime2)

    // Make sure (set!) is memoizing
    val capturedTime3 = eval(datum"""captured-time""")
    assert(capturedTime2 === capturedTime3)
  }

  test("(define-type)") {
    val eval = testEvaluator()

    assert(eval(datum"""(import (llambda typed))""") === "loaded")
    assert(eval(datum"""(define-type <my-int> <integer>)""") === "defined")
    assert(eval(datum"""(define x : <my-int> 10)""") === "x => 10")
    assert(eval(datum"""x""") === "10")
  }

  test("(define-record-type)") {
    val eval = testEvaluator()

    assert(eval(datum"""(define-record-type <record> (record field) record? (field record-field))""") === "defined")
    eval(datum"""(define test-record (record 'value))""")
    assert(eval(datum"""(record-field test-record)""") === "value")
  }

  test("(define-syntax)") {
    val eval = testEvaluator()

    assert(eval(datum"""
      (define-syntax literal-swap
        (syntax-rules ()
          ((my-macro a b) '(b a))))
    """) === "defined")

    assert(eval(datum"""(literal-swap 5 10)""") === "(10 5)")
  }

  test("non-zero exit code") {
    val eval = testEvaluator()

    assert(eval(datum"""(import (llambda process-context))""") === "loaded")

    try {
      eval(datum"""(begin (write 'hello-world) (exit 5))""")
      fail("No exception thrown")
    }
    catch {
      case nonZero : ReplProcessNonZeroExitException =>
        assert(nonZero.code === 5)
        assert(nonZero.stdout === "hello-world")
        assert(nonZero.stderr === "")
    }
  }
}
