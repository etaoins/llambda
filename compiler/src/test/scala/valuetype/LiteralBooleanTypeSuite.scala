package io.llambda.compiler.valuetype


class LiteralBooleanTypeSuite extends SchemeTypeSuite {
  test("the union of both literal booleans is the general boolean") {
    assert(SchemeType.fromTypeUnion(List(literalFalse, literalTrue)) === BooleanType)
  }

  test("general boolean type minus a literal boolean is the other literal boolean") {
    assert((BooleanType - literalTrue) === literalFalse)
    assert((BooleanType - literalFalse) === literalTrue)
  }

  test("intersection of the literal booleans is an empty union") {
    assertIntersection(literalTrue, literalFalse, EmptySchemeType)
  }

  test("intersection of a literal boolean with the general boolean is itself") {
    assertIntersection(literalTrue, BooleanType, literalTrue)
    assertIntersection(literalFalse, BooleanType, literalFalse)
  }

  test("boolean literals satisfy themselvs") {
    assert(SatisfiesType(literalFalse, literalFalse) === Some(true))
    assert(SatisfiesType(literalTrue, literalTrue) === Some(true))
  }

  test("boolean literals satisfy the general boolean type") {
    assert(SatisfiesType(BooleanType, literalFalse) === Some(true))
    assert(SatisfiesType(BooleanType, literalTrue) === Some(true))
  }
}
