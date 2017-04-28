package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}


class SchemeTypeAtomSuite extends SchemeTypeSuite {
  test("creating scheme types from concrete cell types") {
    assert(SchemeType.fromCellType(ct.IntegerCell) ===
      IntegerType
    )
  }

  test("creating scheme types from abstract cell types") {
    assert(SchemeType.fromCellType(ct.NumberCell) ===
      UnionType(Set(IntegerType, FlonumType))
    )
  }

  test("union of two type atoms is a simple union") {
    assert(SchemeType.fromTypeUnion(List(IntegerType, FlonumType)) ===
      UnionType(Set(IntegerType, FlonumType))
    )
  }

  test("union types are flattened when creating a new union") {
    assert(SchemeType.fromTypeUnion(List(IntegerType, UnionType(Set(FlonumType, StringType)))) ===
      UnionType(Set(IntegerType, FlonumType, StringType))
    )
  }

  test("atom types satisfy themselves") {
    assert(SatisfiesType(IntegerType, IntegerType) ===
      Some(true)
    )
  }

  test("atom types satisfy the unit type") {
    assert(ConvertibleToType(UnitType, IntegerType) ===
      Some(true)
    )
  }

  test("atom types definitely don't satisfy other atom types") {
    assert(SatisfiesType(IntegerType, FlonumType) ===
      Some(false)
    )
  }

  test("atom types minus themselves is an empty union") {
    assert((IntegerType - IntegerType) === EmptySchemeType)
  }

  test("atom type minus the empty type is itself") {
    assert((IntegerType - EmptySchemeType) === IntegerType)
  }

  test("atom type minus another atom type is original type") {
    assert((IntegerType - FlonumType) === IntegerType)
  }


  test("atom types intersected with themselves is the original type") {
    assertIntersection(IntegerType, IntegerType, IntegerType)
  }

  test("atom types intersected with another atom is the empty type") {
    assertIntersection(IntegerType, FlonumType, EmptySchemeType)
  }

  test("atom types intersected with the empty type is the empty type") {
    assertIntersection(IntegerType, EmptySchemeType, EmptySchemeType)
  }
}
