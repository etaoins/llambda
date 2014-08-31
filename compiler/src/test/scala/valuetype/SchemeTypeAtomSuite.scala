package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

class SchemeTypeAtomSuite extends SchemeTypeSuite {
  test("creating scheme types from concrete cell types") {
    assert(SchemeType.fromCellType(ct.ExactIntegerCell) ===
      ExactIntegerType
    )
  }
  
  test("creating scheme types from abstract cell types") {
    assert(SchemeType.fromCellType(ct.NumberCell) ===
      UnionType(Set(ExactIntegerType, FlonumType))
    )
  }

  test("union of two type atoms is a simple union") {
    assert(SchemeType.fromTypeUnion(List(ExactIntegerType, FlonumType)) ===
      UnionType(Set(ExactIntegerType, FlonumType))
    )
  }
  
  test("union types are flattened when creating a new union") {
    assert(SchemeType.fromTypeUnion(List(ExactIntegerType, UnionType(Set(FlonumType, StringType)))) ===
      UnionType(Set(ExactIntegerType, FlonumType, StringType))
    )
  }

  test("atom types satisfy themselves") {
    assert(SatisfiesType(ExactIntegerType, ExactIntegerType) ===
      Some(true)
    )
  }
  
  test("atom types definitely don't satisfy other atom types") {
    assert(SatisfiesType(ExactIntegerType, FlonumType) ===
      Some(false)
    )
  }

  test("atom types minus themselves is an empty union") {
    assert((ExactIntegerType - ExactIntegerType) === EmptySchemeType)
  }
  
  test("atom type minus the empty type is itself") {
    assert((ExactIntegerType - EmptySchemeType) === ExactIntegerType)
  }

  test("atom type minus another atom type is original type") {
    assert((ExactIntegerType - FlonumType) === ExactIntegerType)
  }
 

  test("atom types intersected with themselves is the original type") {
    assertIntersection(ExactIntegerType, ExactIntegerType, ExactIntegerType)
  }
  
  test("atom types intersected with another atom is the empty type") {
    assertIntersection(ExactIntegerType, FlonumType, EmptySchemeType)
  }
  
  test("atom types intersected with the empty type is the empty type") {
    assertIntersection(ExactIntegerType, EmptySchemeType, EmptySchemeType)
  }
}
