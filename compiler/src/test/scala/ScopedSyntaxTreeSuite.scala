package io.llambda.compiler

import org.scalatest.FunSuite
import collection.mutable


class ScopedSyntaxTreeSuite  extends FunSuite {
  private object TestLocation extends SourceLocation (
    filenameOpt=None,
    sourceString="NOPE",
    offset=3
  )

  test("scoping and unscoping a non-symbol leaf") {
    val testScope = new Scope(mutable.Map.empty)

    val initialBoolean = ast.Boolean(false)
    initialBoolean.locationOpt = Some(TestLocation)

    val scopedBoolean = sst.ScopedDatum(testScope, initialBoolean)
    assert(scopedBoolean === sst.NonSymbolLeaf(initialBoolean))

    // Make sure the position is preserved
    assert(scopedBoolean.locationOpt === Some(TestLocation))

    val unscopedBoolean = scopedBoolean.unscope
    assert(unscopedBoolean === initialBoolean)
    assert(unscopedBoolean.locationOpt === Some(TestLocation))
  }

  test("scoping and unscoping a symbol") {
    val testScope = new Scope(mutable.Map.empty)

    val initialSymbol = ast.Symbol("Hello, world!")
    initialSymbol.locationOpt = Some(TestLocation)

    val scopedSymbol = sst.ScopedDatum(testScope, initialSymbol)

    assert(scopedSymbol === sst.Symbol(testScope, "Hello, world!"))
    assert(scopedSymbol.locationOpt === Some(TestLocation))

    val unscopedSymbol = scopedSymbol.unscope
    assert(unscopedSymbol === initialSymbol)
    assert(unscopedSymbol.locationOpt === Some(TestLocation))
  }

  test("scoping and unscoping a pair") {
    val testScope = new Scope(mutable.Map.empty)

    val initialPair = ast.Pair(
      ast.Integer(1),
      ast.Symbol("Hello!")
    )

    initialPair.locationOpt = Some(TestLocation)

    val scopedPair = sst.ScopedDatum(testScope, initialPair)

    assert(scopedPair === sst.Pair(
      sst.NonSymbolLeaf(ast.Integer(1)),
      sst.Symbol(testScope, "Hello!")
    ))

    assert(scopedPair.locationOpt === Some(TestLocation))

    val unscopedPair = scopedPair.unscope
    assert(unscopedPair === initialPair)
    assert(unscopedPair.locationOpt === Some(TestLocation))
  }

  test("scoping and unscoping a vector") {
    val testScope = new Scope(mutable.Map.empty)

    val initialVector = ast.Vector(Vector(
      ast.Integer(1),
      ast.Symbol("Hello!")
    ))

    initialVector.locationOpt = Some(TestLocation)

    val scopedVector = sst.ScopedDatum(testScope, initialVector)

    assert(scopedVector === sst.Vector(Vector(
      sst.NonSymbolLeaf(ast.Integer(1)),
      sst.Symbol(testScope, "Hello!")
    )))

    assert(scopedVector.locationOpt === Some(TestLocation))

    val unscopedVector = scopedVector.unscope
    assert(unscopedVector === initialVector)
    assert(unscopedVector.locationOpt === Some(TestLocation))
  }

  test("resolving a scoped symbol") {
    val fooValue = new StorageLocation("foo")
    val testScope = new Scope(mutable.Map("foo" -> fooValue))

    // This exists in our scope
    val fooSymbol = new sst.Symbol(testScope, "foo")
    // This does not exist
    val barSymbol = new sst.Symbol(testScope, "bar")

    // This should be resolvable
    assert(fooSymbol.resolveOpt === Some(fooValue))
    assert(fooSymbol.resolve === fooValue)

    // This should not be resolvable
    assert(barSymbol.resolveOpt === None)
    intercept[UnboundVariableException] {
      barSymbol.resolve
    }
  }
}
