package test.scala

import org.scalatest.FunSuite
import llambda._

import SchemeStringImplicits._

class ParseLibraryNameSuite extends FunSuite { 
  test("single string component") {
    assert(ParseLibraryName(datum"(scheme)") === List(StringComponent("scheme")))
  }
  
  test("single integer component") {
    assert(ParseLibraryName(datum"(1)") === List(IntegerComponent(1)))
  }
  
  test("multiple string component") {
    assert(ParseLibraryName(datum"(scheme core)") === List(StringComponent("scheme"), StringComponent("core")))
  }
  
  test("mixed components") {
    assert(ParseLibraryName(datum"(scheme 1)") === List(StringComponent("scheme"), IntegerComponent(1)))
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
  
  test("zero component failure") {
    intercept[InvalidLibraryNameException] {
      ParseLibraryName(datum"(test 0)")
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
