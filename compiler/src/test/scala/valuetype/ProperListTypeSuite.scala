package io.llambda.compiler.valuetype

import Implicits._


class ProperListTypeSuite extends SchemeTypeSuite {
  test("proper list type satisfies itself") {
    assert(SatisfiesType(stringList, stringList) === Some(true))
  }

  test("proper list type satisfies its unrolled version") {
    assert(SatisfiesType(stringList.unrolled, stringList) === Some(true))
  }

  test("unrolled proper list type satisfies its rolled version") {
    assert(SatisfiesType(stringList, stringList.unrolled) === Some(true))
  }

  test("proper list type satisfies list element type") {
    assert(SatisfiesType(ListElementType, stringList) === Some(true))
  }

  test("list element type may satisfy proper list type") {
    assert(SatisfiesType(stringList, ListElementType) === None)
  }

  test("proper list type satisfies more general proper list") {
    assert(SatisfiesType(numericList, integerList) === Some(true))
  }

  test("proper list type may satisfy more specific proper list") {
    assert(SatisfiesType(integerList, numericList) === None)
  }

  test("proper list may satisfy disjoint proper list") {
    // This is because two "typed" empty lists will satisfy each other
    assert(SatisfiesType(stringList, integerList) === None)
  }

  test("empty list definitely satisfies proper list") {
    assert(SatisfiesType(stringList, EmptyListType) === Some(true))
  }

  test("known list structure satisfies proper list") {
    assert(SatisfiesType(numericList, knownNumberList) === Some(true))
  }

  test("proper list may satisfy empty list") {
    assert(SatisfiesType(EmptyListType, stringList) === None)
  }

  test("proper list may satisfy compatible pair") {
    assert(SatisfiesType(knownNumberList, numericList) === None)
    assert(SatisfiesType(AnyPairType, numericList) === None)
  }

  test("proper list definitely does not satisfy incompatible pair") {
    assert(SatisfiesType(knownNumberList, stringList) === Some(false))
  }

  test("proper list type definitely doesn't satisfy non-list element") {
    assert(SatisfiesType(IntegerType, numericList) === Some(false))
  }

  test("non-list element doesn't satisfy a proper list") {
    assert(SatisfiesType(numericList, IntegerType) === Some(false))
  }

  test("proper list type minus a compatible list is the empty union") {
    assert((integerList - numericList) == EmptySchemeType)
  }

  test("proper list type minus the empty type is itself") {
    assert((integerList - EmptySchemeType) == integerList)
  }

  test("proper list type minus an incompatible list is a pair with a proper list cdr") {
    assert((integerList - stringList) == PairType(IntegerType, integerList))
  }

  test("proper list type minus the empty list type is its pair type") {
    assert((stringList - EmptyListType) == PairType(StringType, stringList))
  }

  test("proper list type minus a compatible pair type is the empty list") {
    assert((stringList - AnyPairType) == EmptyListType)
  }

  test("string proper list minus a non-empty string proper list is an empty list") {
    assert((stringList - nonEmptyProperList(StringType)) === EmptyListType)
  }

  test("proper list type minus an incompatible pair type is itself") {
    assert((stringList - PairType(SymbolType, StringType)) == stringList)
  }

  test("proper list type minus the list element type is the empty union") {
    assert((stringList - ListElementType) == EmptySchemeType)
  }

  test("the list element type minus a proper list is a pair") {
    // EmptyListType is a proper list so this must be a pair
    assert((ListElementType - stringList) == AnyPairType)
  }

  test("proper list type minus an unrelated type is itself") {
    assert((integerList - PortType) == integerList)
  }

  test("proper list type intersected with a compatible list is the most specific list") {
    assertIntersection(integerList, numericList, integerList)
  }

  test("proper list type intersected with an incompatible list is an empty list") {
    assertIntersection(integerList, stringList, EmptyListType)
  }

  test("proper list type intersected with the empty list is an empty list") {
    assertIntersection(stringList, EmptyListType, EmptyListType)
  }

  test("proper list type intersected with a compatible pair is its pair type") {
    assertIntersection(stringList, AnyPairType, PairType(StringType, stringList))
  }

  test("proper list type intersected with an incompatible pair is an empty union") {
    assertIntersection(stringList, PairType(SymbolType, StringType), EmptySchemeType)
  }

  test("proper list type intersected with the list element type is itself") {
    assertIntersection(stringList, ListElementType, stringList)
  }

  test("proper list type intersected with an unrelated type is an empty union") {
    assertIntersection(stringList, PortType, EmptySchemeType)
  }

  test("string proper list may satisfy non-empty string proper list") {
    assert(SatisfiesType(nonEmptyProperList(StringType), UniformProperListType(StringType)) === None)
  }

  test("non-empty string proper list definitely satisfies string proper list") {
    assert(SatisfiesType(UniformProperListType(StringType), nonEmptyProperList(StringType)) === Some(true))
  }
}
