package io.llambda.compiler.valuetype


class LiteralSymbolTypeSuite extends SchemeTypeSuite {
  val symbolA = LiteralSymbolType("A")
  val symbolB = LiteralSymbolType("B")
  val symbolC = LiteralSymbolType("C")
  val symbolD = LiteralSymbolType("D")

  val unionAB = SchemeType.fromTypeUnion(List(symbolA, symbolB))
  val unionBC = SchemeType.fromTypeUnion(List(symbolB, symbolC))
  val unionCD = SchemeType.fromTypeUnion(List(symbolC, symbolD))

  test("literal symbol definitely satisfies itself") {
    assert(SatisfiesType(symbolB, symbolB) ===
      Some(true)
    )
  }

  test("literal symbol definitely satisfy the general symbol") {
    assert(SatisfiesType(SymbolType, symbolC) ===
      Some(true)
    )
  }

  test("the general symbol may satisfy a literal symbol") {
    assert(SatisfiesType(symbolD, SymbolType) ===
      None
    )
  }

  test("literal symbol definitely does not satisfy other symbol") {
    assert(SatisfiesType(symbolA, symbolB) ===
      Some(false)
    )
  }

  test("intersection of disjoint symbols is an empty union") {
    assertIntersection(unionAB, unionCD, EmptySchemeType)
  }

  test("intersection of overlapping symbols is their common symbols") {
    assertIntersection(unionAB, unionBC, symbolB)
  }

  test("intersection of a literal symbol with the general symbol is itself ") {
    assertIntersection(symbolA, SymbolType, symbolA)
  }
}
