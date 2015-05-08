package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite
import llambda.compiler._

import SchemeStringImplicits._

class ParseLibraryNameSuite extends FunSuite {
  test("single string component") {
    assert(ParseLibraryName(datum"(scheme)") === List("scheme"))
  }

  test("positive integer component") {
    assert(ParseLibraryName(datum"(1)") === List("1"))
  }

  test("zero integer component") {
    assert(ParseLibraryName(datum"(0)") === List("0"))
  }

  test("multiple string component") {
    assert(ParseLibraryName(datum"(scheme base)") === List("scheme", "base"))
  }

  test("mixed components") {
    assert(ParseLibraryName(datum"(scheme 1)") === List("scheme", "1"))
  }

  test("no components failure") {
    intercept[InvalidLibraryNameException] {
      ParseLibraryName(datum"()")
    }
  }

  test("negative integer component failure") {
    intercept[InvalidLibraryNameException] {
      ParseLibraryName(datum"(test -1)")
    }
  }

  test("string literal component failure") {
    intercept[InvalidLibraryNameException] {
      ParseLibraryName(datum"""(test "hello")""")
    }
  }

  test("improper list component failure") {
    intercept[InvalidLibraryNameException] {
      ParseLibraryName(datum"(test (1 . 2))")
    }
  }
}
