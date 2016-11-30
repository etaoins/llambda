package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite, Inside}
import llambda.compiler._

import SchemeStringImplicits._

class ResolveIncludeListSuite extends FunSuite with Inside { 
  val resourceBaseUrl = getClass.getClassLoader.getResource("")

  implicit val includePath = frontend.IncludePath(List(resourceBaseUrl))

  test("zero includes fails") {
    intercept[BadSpecialFormException] {
      ResolveIncludeList(NoSourceLocation, Nil)
    }
  }

  test("including a symbol fails") {
    intercept[BadSpecialFormException] {
      ResolveIncludeList(NoSourceLocation, List(ast.Symbol("test")))
    }
  }

  test("including an integer fails") {
    intercept[BadSpecialFormException] {
      ResolveIncludeList(NoSourceLocation, List(ast.IntegerLiteral(1)))
    }
  }

  test("including a proper list fails") {
    intercept[BadSpecialFormException] {
      ResolveIncludeList(NoSourceLocation, List(
        ast.StringLiteral("includes/include1.scm"),
        ast.ProperList(List(
          ast.StringLiteral("World")
        ))
      ))
    }
  }

  test("including a missing file fails") {
    intercept[IncludeNotFoundException] {
      ResolveIncludeList(NoSourceLocation, List(ast.StringLiteral("doesntexist.scm")))
    }
  }


  test("including a single file") {
    val data = ResolveIncludeList(NoSourceLocation, List(
      ast.StringLiteral("includes/include1.scm")
    ))

    assert(data === List(
      ast.StringLiteral("include1-line1"),
      ast.StringLiteral("include1-line2")
    ))
  }

  test("including multiple files") {
    val data = ResolveIncludeList(NoSourceLocation, List(
      ast.StringLiteral("includes/include1.scm"),
      ast.StringLiteral("includes/include2.scm")
    ))

    assert(data === List(
      ast.StringLiteral("include1-line1"),
      ast.StringLiteral("include1-line2"),
      ast.StringLiteral("include2-line1"),
      ast.StringLiteral("include2-line2")
    ))
  }
}
