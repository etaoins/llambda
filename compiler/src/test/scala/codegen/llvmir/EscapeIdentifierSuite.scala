package io.llambda.compiler.codegen.llvmir
import io.llambda

import org.scalatest.FunSuite

class EscapeIdentifierSuite extends FunSuite {
  test("trivial identifier") {
    assert(EscapeIdentifier("hello") === "hello")
  }
  
  test("identifier with underscores") {
    assert(EscapeIdentifier("hello_world") === "hello_world")
  }
  
  test("identifier with spaces") {
    assert(EscapeIdentifier("hello world") === "\"hello world\"")
  }
  
  test("identifier with quotes") {
    assert(EscapeIdentifier("hello\"world") === "\"hello\\\"world\"")
  }
  
  test("identifier with backslash") {
    assert(EscapeIdentifier("hello\\world") === "\"hello\\\\world\"")
  }

  test("identifier with question mark") {
    assert(EscapeIdentifier("boolean?") === "\"boolean?\"")
  }
  
  test("identifier with hyphens and exclaimation mark") {
    assert(EscapeIdentifier("set-cdr!") === "\"set-cdr!\"")
  }
}
