package io.llambda.compiler
import io.llambda

import org.scalatest.FunSuite
import llambda.compiler.SchemeStringImplicits._

class ReplSuite extends FunSuite {
  private val targetPlatform = platform.DetectJvmPlatform()

  private def testRepl() : Repl =
    new Repl(targetPlatform, dialect.Dialect.default)

  test("evaluating a trivial expression") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(+ 1 2)""") === "3")
  }

  test("evaluting an expression returning multiple values") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(exact-integer-sqrt 18)""") === "(values 4 2)")
  }

  test("simple (define)") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(define x 2)""") === "x => 2")
    assert(repl.evalDatum(datum"""2""") === "2")
  }

  test("(define) is identified by binding") {
    val repl = testRepl()

    // Try renaming (define)
    assert(repl.evalDatum(datum"""(import (rename (scheme base) (define my-define)))""") === "loaded")

    assert(repl.evalDatum(datum"""(my-define x 2)""") === "x => 2")
    assert(repl.evalDatum(datum"""2""") === "2")
  }

  test("typed (define)") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(import (llambda typed))""") === "loaded")
    assert(repl.evalDatum(datum"""(define x : <exact-integer> 10)""") === "x => 10")
    assert(repl.evalDatum(datum"""x""") === "10")
  }

  test("lambda shorthand (define)") {
    val repl = testRepl()

    repl.evalDatum(datum"""(define (return-both x y) (values x y))""")
    assert(repl.evalDatum(datum"""(return-both 10 20)""") === "(values 10 20)")
  }

  test("(define) memoizes Scheme data") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(import (scheme time))""") === "loaded")

    repl.evalDatum(datum"""(define captured-time (current-jiffy))""")

    // Make sure we're not evaluating (current-jiffy) multiple times
    val capturedTime1 = repl.evalDatum(datum"""captured-time""")
    val capturedTime2 = repl.evalDatum(datum"""captured-time""")

    assert(capturedTime1 === capturedTime2)
  }

  test("failed (define) doesnt leave binding") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(import (llambda typed))""") === "loaded")
    assert(repl.evalDatum(datum"""(import (scheme read))""") === "loaded")

    intercept[ReplProcessNonZeroExitException] {
      // 1.5 isn't an exact integer
      repl.evalDatum(datum"""(define x : <exact-integer> (string->number "1.5"))""")
    }

    intercept[UnboundVariableException] {
      // Make sure x isn't bound to anything
      assert(repl.evalDatum(datum"""x""") === "10")
    }
  }

  test("simple (set!)") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(import (scheme time))""") === "loaded")

    // Define (captured-time) to one value
    repl.evalDatum(datum"""(define captured-time (current-jiffy))""")
    val capturedTime1 = repl.evalDatum(datum"""captured-time""")

    // Set it to another value
    repl.evalDatum(datum"""(set! captured-time (current-jiffy))""")
    val capturedTime2 = repl.evalDatum(datum"""captured-time""")

    // Make sure they're distinct
    assert(capturedTime1 !== capturedTime2)

    // Make sure (set!) is memoizing
    val capturedTime3 = repl.evalDatum(datum"""captured-time""")
    assert(capturedTime2 === capturedTime3)
  }

  test("(define-type)") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(import (llambda typed))""") === "loaded")
    assert(repl.evalDatum(datum"""(define-type <my-int> <exact-integer>)""") === "defined")
    assert(repl.evalDatum(datum"""(define x : <my-int> 10)""") === "x => 10")
    assert(repl.evalDatum(datum"""x""") === "10")
  }

  test("(define-record-type)") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(define-record-type <record> (record field) record? (field record-field))""") === "defined")
    repl.evalDatum(datum"""(define test-record (record 'value))""")
    assert(repl.evalDatum(datum"""(record-field test-record)""") === "value")
  }

  test("(define-syntax)") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""
      (define-syntax literal-swap
        (syntax-rules ()
          ((my-macro a b) '(b a))))
    """) === "defined")

    assert(repl.evalDatum(datum"""(literal-swap 5 10)""") === "(10 5)")
  }

  test("non-zero exit code") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(import (scheme process-context))""") === "loaded")

    try {
      repl.evalDatum(datum"""(begin (write 'hello-world) (exit 5))""")
      fail("No exception thrown")
    }
    catch {
      case nonZero : ReplProcessNonZeroExitException =>
        assert(nonZero.code === 5)
        assert(nonZero.output === "hello-world")
    }
  }
}
