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

  test("simple (define)") {
    val repl = testRepl()

    assert(repl.evalDatum(datum"""(define x 2)""") === "x => 2")
    assert(repl.evalDatum(datum"""2""") === "2")
  }

  test("non-zero exit code") {
    val repl = testRepl()

    repl.evalDatum(datum"""(import (scheme process-context))""")

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
