package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

class UnionTypeSuite extends SchemeTypeSuite {
  test("atom types definitely satisfy a union containing themselves") {
    val unionWithExactInt = UnionType(Set(ExactIntegerType, FlonumType))

    assert(SatisfiesType(unionWithExactInt, ExactIntegerType) ===
      Some(true)
    )
  }
  
  test("atom types definitely don't satisfy a union not containing themselves") {
    val unionWithoutExactInt = UnionType(Set(FlonumType, FlonumType))

    assert(SatisfiesType(unionWithoutExactInt, ExactIntegerType) ===
      Some(false)
    )
  }

  test("the empty type definitely satisfies the any type") {
    assert(SatisfiesType(AnySchemeType, EmptySchemeType) === Some(true))
  }
  
  test("the any type definitely does not satisfy the empty type") {
    assert(SatisfiesType(EmptySchemeType, AnySchemeType) === Some(false))
  }
  
  test("the empty type definitely satisfies itself") {
    assert(SatisfiesType(EmptySchemeType, EmptySchemeType) === Some(true))
  }

  test("union types definitely satisfy superset unions") {
    val subunion = UnionType(Set(FlonumType, ExactIntegerType))
    val superunion = UnionType(Set(StringType, FlonumType, ExactIntegerType))

    assert(SatisfiesType(superunion, subunion) === Some(true))
  }
  
  test("union types definitely don't satisfy disjoint unions") {
    val union1 = UnionType(Set(FlonumType, ExactIntegerType))
    val union2 = UnionType(Set(StringType))

    assert(SatisfiesType(union1, union2) === Some(false))
    assert(SatisfiesType(union2, union1) === Some(false))
  }
  
  test("union types definitely don't satisfy the empty type") {
    val nonEmptyUnion = UnionType(Set(FlonumType, ExactIntegerType))

    assert(SatisfiesType(EmptySchemeType, nonEmptyUnion) === Some(false))
  }
  
  test("the empty scheme type definitely satisfies union types") {
    val nonEmptyUnion = UnionType(Set(FlonumType, ExactIntegerType))

    assert(SatisfiesType(nonEmptyUnion, EmptySchemeType) === Some(true))
  }
  
  test("union types may satisfy intersecting union type") {
    val union1 = UnionType(Set(FlonumType, ExactIntegerType))
    val union2 = UnionType(Set(StringType, FlonumType))

    assert(SatisfiesType(union1, union2) === None)
    assert(SatisfiesType(union2, union1) === None)
  }
  
  test("union type minus intersecting union type is the remaining types") {
    val union1 = UnionType(Set(FlonumType, ExactIntegerType))
    val union2 = UnionType(Set(StringType, FlonumType))

    assert((union1 - union2) === ExactIntegerType)
  }
  
  test("union type minus the empty type is itself") {
    val nonEmptyUnion = UnionType(Set(FlonumType, ExactIntegerType))

    assert((nonEmptyUnion - EmptySchemeType) === nonEmptyUnion)
  }
  
  test("union type minus an atom type removes that type from the union") {
    assert((UnionType(Set(ExactIntegerType, FlonumType, StringType)) - ExactIntegerType) ===
      UnionType(Set(FlonumType, StringType))
    )
  }

  test("union type with record type minus record atom removes the record type") {
    assert((UnionType(Set(ExactIntegerType, recordInstance1)) - recordAtomType) ===
      ExactIntegerType
    )
  }
  
  test("union type with record atom minus record type does not remove the record atom") {
    assert((UnionType(Set(ExactIntegerType, recordAtomType)) - recordInstance2) ===
      UnionType(Set(ExactIntegerType, recordAtomType))
    )
  }

  test("atom types intersected with a union containing the atom is the original atom") {
    assertIntersection(ExactIntegerType, UnionType(Set(ExactIntegerType, StringType)), ExactIntegerType)
  }
  
  test("atom types intersected with a union not containing the atom is an empty union") {
    assertIntersection(FlonumType, UnionType(Set(ExactIntegerType, StringType)), EmptySchemeType)
  }

  test("two union types intersected is the common member") {
    assertIntersection(UnionType(Set(FlonumType, ExactIntegerType)), UnionType(Set(ExactIntegerType, StringType)), ExactIntegerType)
  }
  
  test("union types intersected with the empty type is the empty type") {
    assertIntersection(UnionType(Set(FlonumType, ExactIntegerType)), EmptySchemeType, EmptySchemeType)
  }

  test("the cell type for the union of exact int and inexact is numeric") {
    assert(UnionType(Set(ExactIntegerType, FlonumType)).cellType === ct.NumberCell)
  }
  
  test("the cell type for the union of exact int, inexact and string is datum") {
    assert(UnionType(Set(ExactIntegerType, FlonumType, StringType)).cellType === ct.AnyCell)
  }

  test("placing a recursive union inside another union unrolls the recursive union") {
    // See the comment in fromTypeUnion for more information about this
    val expected = UnionType(Set(
      literalFalse,
      EmptyListType,
      PairType(
        ExactIntegerType,
        UnionType(Set(
          EmptyListType,
          PairType(
            ExactIntegerType,
            RecursiveSchemeTypeRef(1)
          )
        ))
      )
    ))

    assert(SchemeType.fromTypeUnion(List(exactIntList, literalFalse)) === expected)
  }
}
