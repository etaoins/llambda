package llambda

import org.scalatest.FunSuite
import util.parsing.input.Position
import collection.mutable

class ScopedSyntaxTreeSuite  extends FunSuite {
  private object TestPosition extends Position {
    def line : Int = 42
    def column : Int = 23
    def lineContents = "NOPE"
  }

  test("scoping and unscoping a non-symbol leaf") {
    val testScope = new Scope(mutable.Map.empty)

    val initialBoolean = ast.BooleanLiteral(false)
    initialBoolean.setPos(TestPosition)

    val scopedBoolean = sst.ScopedDatum(testScope, initialBoolean)
    assert(scopedBoolean === sst.NonSymbolLeaf(initialBoolean))
    
    // Make sure the position is preserved
    assert(scopedBoolean.pos === TestPosition)

    val unscopedBoolean = scopedBoolean.unscope
    assert(unscopedBoolean === initialBoolean)
    assert(unscopedBoolean.pos === TestPosition)
  }

  test("scoping and unscoping a symbol") {
    val testScope = new Scope(mutable.Map.empty)

    val initialSymbol = ast.Symbol("Hello, world!")
    initialSymbol.setPos(TestPosition)

    val scopedSymbol = sst.ScopedDatum(testScope, initialSymbol)

    assert(scopedSymbol === sst.ScopedSymbol(testScope, "Hello, world!"))
    assert(scopedSymbol.pos === TestPosition)
    
    val unscopedSymbol = scopedSymbol.unscope
    assert(unscopedSymbol === initialSymbol)
    assert(unscopedSymbol.pos === TestPosition)
  }
  
  test("scoping and unscoping a pair") {
    val testScope = new Scope(mutable.Map.empty)

    val initialPair = ast.Pair(
      ast.IntegerLiteral(1),
      ast.Symbol("Hello!")
    )

    initialPair.setPos(TestPosition)

    val scopedPair = sst.ScopedDatum(testScope, initialPair)

    assert(scopedPair === sst.ScopedPair(
      sst.NonSymbolLeaf(ast.IntegerLiteral(1)),
      sst.ScopedSymbol(testScope, "Hello!") 
    ))

    assert(scopedPair.pos === TestPosition)
    
    val unscopedPair = scopedPair.unscope
    assert(unscopedPair === initialPair)
    assert(unscopedPair.pos === TestPosition)
  }
  
  test("scoping and unscoping a vector") {
    val testScope = new Scope(mutable.Map.empty)

    val initialVector = ast.VectorLiteral(Vector(
      ast.IntegerLiteral(1),
      ast.Symbol("Hello!")
    ))

    initialVector.setPos(TestPosition)

    val scopedVector = sst.ScopedDatum(testScope, initialVector)

    assert(scopedVector === sst.ScopedVectorLiteral(Vector(
      sst.NonSymbolLeaf(ast.IntegerLiteral(1)),
      sst.ScopedSymbol(testScope, "Hello!") 
    )))

    assert(scopedVector.pos === TestPosition)
    
    val unscopedVector = scopedVector.unscope
    assert(unscopedVector === initialVector)
    assert(unscopedVector.pos === TestPosition)
  }

  test("resolving a scoped symbol") {
    val fooValue = new StorageLocation("foo")
    val testScope = new Scope(mutable.Map("foo" -> fooValue))

    // This exists in our scope
    val fooSymbol = new sst.ScopedSymbol(testScope, "foo")
    // This does not exist
    val barSymbol = new sst.ScopedSymbol(testScope, "bar")

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
