package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

class UnionTypeSuite extends SchemeTypeSuite {
  test("atom types definitely satisfy a union containing themselves") {
    val unionWithInteger = UnionType(Set(IntegerType, FlonumType))

    assert(SatisfiesType(unionWithInteger, IntegerType) ===
      Some(true)
    )
  }
  
  test("atom types definitely don't satisfy a union not containing themselves") {
    val unionWithoutInteger = UnionType(Set(FlonumType, FlonumType))

    assert(SatisfiesType(unionWithoutInteger, IntegerType) ===
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
    val subunion = UnionType(Set(FlonumType, IntegerType))
    val superunion = UnionType(Set(StringType, FlonumType, IntegerType))

    assert(SatisfiesType(superunion, subunion) === Some(true))
  }
  
  test("union types definitely don't satisfy disjoint unions") {
    val union1 = UnionType(Set(FlonumType, IntegerType))
    val union2 = UnionType(Set(StringType))

    assert(SatisfiesType(union1, union2) === Some(false))
    assert(SatisfiesType(union2, union1) === Some(false))
  }
  
  test("union types definitely don't satisfy the empty type") {
    val nonEmptyUnion = UnionType(Set(FlonumType, IntegerType))

    assert(SatisfiesType(EmptySchemeType, nonEmptyUnion) === Some(false))
  }
  
  test("the empty scheme type definitely satisfies union types") {
    val nonEmptyUnion = UnionType(Set(FlonumType, IntegerType))

    assert(SatisfiesType(nonEmptyUnion, EmptySchemeType) === Some(true))
  }
  
  test("union types may satisfy intersecting union type") {
    val union1 = UnionType(Set(FlonumType, IntegerType))
    val union2 = UnionType(Set(StringType, FlonumType))

    assert(SatisfiesType(union1, union2) === None)
    assert(SatisfiesType(union2, union1) === None)
  }
  
  test("union type minus intersecting union type is the remaining types") {
    val union1 = UnionType(Set(FlonumType, IntegerType))
    val union2 = UnionType(Set(StringType, FlonumType))

    assert((union1 - union2) === IntegerType)
  }
  
  test("union type minus the empty type is itself") {
    val nonEmptyUnion = UnionType(Set(FlonumType, IntegerType))

    assert((nonEmptyUnion - EmptySchemeType) === nonEmptyUnion)
  }
  
  test("union type minus an atom type removes that type from the union") {
    assert((UnionType(Set(IntegerType, FlonumType, StringType)) - IntegerType) ===
      UnionType(Set(FlonumType, StringType))
    )
  }

  test("union type with record type minus record atom removes the record type") {
    assert((UnionType(Set(IntegerType, recordType1)) - recordAtomType) ===
      IntegerType
    )
  }
  
  test("union type with record atom minus record type does not remove the record atom") {
    assert((UnionType(Set(IntegerType, recordAtomType)) - recordType2) ===
      UnionType(Set(IntegerType, recordAtomType))
    )
  }

  test("atom types intersected with a union containing the atom is the original atom") {
    assertIntersection(IntegerType, UnionType(Set(IntegerType, StringType)), IntegerType)
  }
  
  test("atom types intersected with a union not containing the atom is an empty union") {
    assertIntersection(FlonumType, UnionType(Set(IntegerType, StringType)), EmptySchemeType)
  }

  test("two union types intersected is the common member") {
    assertIntersection(UnionType(Set(FlonumType, IntegerType)), UnionType(Set(IntegerType, StringType)), IntegerType)
  }
  
  test("union types intersected with the empty type is the empty type") {
    assertIntersection(UnionType(Set(FlonumType, IntegerType)), EmptySchemeType, EmptySchemeType)
  }

  test("the cell type for the union of integer and flonum is numeric") {
    assert(UnionType(Set(IntegerType, FlonumType)).cellType === ct.NumberCell)
  }
  
  test("the cell type for the union of integer, flonum and string is datum") {
    assert(UnionType(Set(IntegerType, FlonumType, StringType)).cellType === ct.AnyCell)
  }

  test("placing a recursive union inside another union unrolls the recursive union") {
    // See the comment in fromTypeUnion for more information about this
    val expected = UnionType(Set(
      literalFalse,
      EmptyListType,
      PairType(
        IntegerType,
        UnionType(Set(
          EmptyListType,
          PairType(
            IntegerType,
            RecursiveSchemeTypeRef(1)
          )
        ))
      )
    ))

    assert(SchemeType.fromTypeUnion(List(integerList, literalFalse)) === expected)
  }
}
