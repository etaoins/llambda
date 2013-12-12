package io.llambda.llvmir

import org.scalatest.FunSuite

class IrNameSourceSuite extends FunSuite {
  test("generating local names") {
    val source = new LocalNameSource

    assert(source.allocate("foo") === "foo1")
    assert(source.allocate("foo") === "foo2")
    assert(source.allocate("bar") === "bar1")
    assert(source.allocate("foo") === "foo3")
  }
  
  test("generating global names") {
    val source = new GlobalNameSource

    assert(source.allocate("foo") === "foo1")
    assert(source.allocate("foo") === "foo2")
    assert(source.allocate("bar") === "bar1")
    assert(source.allocate("foo") === "foo3")
  }
}
