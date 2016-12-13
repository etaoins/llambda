package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,OptionValues}

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._

class ExtractPatternSuite extends FunSuite with OptionValues with testutil.ExprHelpers {
  implicit val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq: _*))

  test("empty pattern match results in an expression") {
    exprFor("(match #t)")
  }

  test("one clause match results in an expression") {
    exprFor("(match #t (1 2))")
  }

  test("empty clause results in an error") {
    intercept[BadSpecialFormException] {
      exprFor("(match #t ())")
    }
  }

  test("unapplying an unbound value fails") {
    intercept[UnboundVariableException] {
      exprFor("(match #t ((unbound 1 2) 'false))")
    }
  }

  test("unsupported unapplication results in an error") {
    intercept[BadSpecialFormException] {
      exprFor("(match #t ((lambda () #t) 'foo))")
    }
  }

  test("unapplying a record type with the wrong arugment count fails") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    // Create the record type
    bodyFor("""
      (define-record-type <two-fields> (two-fields first second) two-fields?
        (first two-fields-first)
        (second two-fields-second))
    """)(scope)

    intercept[BadSpecialFormException] {
      bodyFor("(match #t ((two-fields) 'error))")(scope)
    }

    intercept[BadSpecialFormException] {
      bodyFor("(match #t ((two-fields first) 'error))")(scope)
    }

    intercept[BadSpecialFormException] {
      bodyFor("(match #t ((two-fields first second third) 'error))")(scope)
    }
  }
}
