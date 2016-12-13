package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

class PairTypeSuite extends SchemeTypeSuite {
  protected def binaryTreeType(memberType: SchemeType): SchemeType =
    SchemeType.fromTypeUnion(List(
      memberType,
      SpecificPairType(
        RecursiveSchemeTypeRef(1),
        RecursiveSchemeTypeRef(1)
      )
    ))

  protected def infiniteListType(memberType: SchemeType): SchemeType =
    SpecificPairType(
      memberType,
      RecursiveSchemeTypeRef(0)
    )

  test("the any pair type satisfies itself") {
    assert(SatisfiesType(AnyPairType, AnyPairType) === Some(true))
  }

  test("specific pair type satisfies the any pair type") {
    val specificPairType = PairType(SymbolType, StringType)
    assert(SatisfiesType(AnyPairType, specificPairType) === Some(true))
  }

  test("the any pair type may satisfy a specific pair type") {
    val specificPairType = PairType(SymbolType, StringType)
    assert(SatisfiesType(specificPairType, AnyPairType) === None)
  }

  test("the any pair type is convertable to the unit type") {
    assert(ConvertibleToType(UnitType, AnyPairType) === Some(true))
  }

  test("union with any pair type intersected with union with specific pair type is the specific pair type union") {
    val anyPairUnion = UnionType(Set(EmptyListType, AnyPairType))
    val specificPairUnion = UnionType(Set(EmptyListType, PairType(SymbolType, StringType)))

    assertIntersection(anyPairUnion, specificPairUnion, specificPairUnion)
  }

  test("completely incompatible specific pair types do not satisfy each other") {
    val specificPairType1 = PairType(SymbolType, StringType)
    val specificPairType2 = PairType(StringType, SymbolType)

    assert(SatisfiesType(specificPairType1, specificPairType2) === Some(false))
  }

  test("partially compatible specific pair types do not satisfy each other") {
    // car may satisfy, cdr does not satisfy
    val specificPairType1 = PairType(IntegerType, StringType)
    val specificPairType2 = PairType(NumberType, SymbolType)

    assert(SatisfiesType(specificPairType1, specificPairType2) === Some(false))
  }

  test("integer binary tree definitely satisfies itself") {
    assert(SatisfiesType(binaryTreeType(IntegerType), binaryTreeType(IntegerType)) === Some(true))
  }

  test("integer binary tree definitely satisfies number binary tree") {
    assert(SatisfiesType(binaryTreeType(NumberType), binaryTreeType(IntegerType)) === Some(true))
  }

  test("number binary tree may satisfy integer binary tree") {
    assert(SatisfiesType(binaryTreeType(IntegerType), binaryTreeType(NumberType)) === None)
  }

  test("binary tree type minus itself is the empty type") {
    assert((binaryTreeType(SymbolType) - binaryTreeType(SymbolType)) === EmptySchemeType)
  }

  test("intersection of a binary tree and its member type is the member type") {
    assertIntersection(binaryTreeType(PortType), PortType, PortType)
  }

  test("intersection of two binary trees is the most specific binary tree") {
    assertIntersection(binaryTreeType(NumberType), binaryTreeType(FlonumType), binaryTreeType(FlonumType))
  }

  test("empty list does not satisfy an infinite list") {
    assert(SatisfiesType(infiniteListType(StringType), EmptyListType) === Some(false))
  }

  test("proper list may satisify an infinite list") {
    assert(SatisfiesType(infiniteListType(StringType), stringList) === None)
  }

  test("non-empty proper list may satisify an infinite list") {
    assert(SatisfiesType(infiniteListType(StringType), nonEmptyProperList(StringType)) === None)
  }

  test("infinite list definitely satisfies a proper list") {
    assert(SatisfiesType(stringList, infiniteListType(StringType)) === Some(true))
  }

  test("infinite list definitely satisfies a non-empty list") {
    assert(SatisfiesType(nonEmptyProperList(StringType), infiniteListType(StringType)) === Some(true))
  }

  test("intersection of two infinite lists is the most specific list") {
    assertIntersection(infiniteListType(NumberType), infiniteListType(FlonumType), infiniteListType(FlonumType))
  }
}
