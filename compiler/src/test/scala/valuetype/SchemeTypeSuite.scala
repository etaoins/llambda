package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}

class SchemeTypeSuite extends FunSuite {
  private val recordAtomType = SchemeTypeAtom(ct.RecordCell)

  private val recordType1 = new RecordType("record1", Nil)
  private val recordType2 = new RecordType("record1", Nil)

  private val constantTrue = ConstantBooleanType(true)
  private val constantFalse = ConstantBooleanType(false)

  private def assertIntersection(type1 : SchemeType, type2 : SchemeType, resultType : SchemeType) {
    assert((type1 & type2) === resultType) 
    assert((type2 & type1) === resultType) 
  }

  test("creating scheme types from concrete cell types") {
    assert(SchemeType.fromCellType(ct.ExactIntegerCell) ===
      ExactIntegerType
    )
  }
  
  test("creating scheme types from abstract cell types") {
    assert(SchemeType.fromCellType(ct.NumericCell) ===
      UnionType(Set(ExactIntegerType, InexactRationalType))
    )
  }

  test("union of two type atoms is a simple union") {
    assert(SchemeType.fromTypeUnion(List(ExactIntegerType, InexactRationalType)) ===
      UnionType(Set(ExactIntegerType, InexactRationalType))
    )
  }
  
  test("union types are flattened when creating a new union") {
    assert(SchemeType.fromTypeUnion(List(ExactIntegerType, UnionType(Set(InexactRationalType, StringType)))) ===
      UnionType(Set(ExactIntegerType, InexactRationalType, StringType))
    )
  }

  test("atom types definitely satisfy themselves") {
    assert(ExactIntegerType.satisfiesType(ExactIntegerType) ===
      Some(true)
    )
  }
  
  test("atom types definitely don't satisfy other atom types") {
    assert(ExactIntegerType.satisfiesType(InexactRationalType) ===
      Some(false)
    )
  }

  test("record types definitely satisfy themselves") {
    assert(recordType1.satisfiesType(recordType1) ===
      Some(true)
    )
  }
  
  test("record types definitely don't satisfy other atom types") {
    assert(recordType1.satisfiesType(recordType2) ===
      Some(false)
    )
  }
  
  test("record types definitely satisfy the record atom") {
    assert(recordType1.satisfiesType(recordAtomType) ===
      Some(true)
    )
  }

  test("the record atom may satisfy a record type") {
    assert(recordAtomType.satisfiesType(recordType2) ===
      None
    )
  }

  test("atom types definitely satisfy a union containing themselves") {
    assert(ExactIntegerType.satisfiesType(UnionType(Set(ExactIntegerType, InexactRationalType))) ===
      Some(true)
    )
  }
  
  test("atom types definitely don't satisfy a union not containing themselves") {
    assert(ExactIntegerType.satisfiesType(UnionType(Set(StringType, InexactRationalType))) ===
      Some(false)
    )
  }

  test("union types definitely satisfy superset unions") {
    assert(UnionType(Set(InexactRationalType, ExactIntegerType)).satisfiesType(UnionType(Set(StringType, InexactRationalType, ExactIntegerType))) ===
      Some(true)
    )
  }
  
  test("union types definitely don't satisfy disjoint unions") {
    assert(UnionType(Set(InexactRationalType, ExactIntegerType)).satisfiesType(UnionType(Set(StringType))) ===
      Some(false)
    )
  }
  
  test("union types may satisfy intersecting union type") {
    assert(UnionType(Set(InexactRationalType, ExactIntegerType)).satisfiesType(UnionType(Set(StringType, InexactRationalType))) ===
      None
    )
  }

  test("atom types minus themselves is an empty union") {
    assert((ExactIntegerType - ExactIntegerType) === UnionType(Set()))
  }

  test("atom types minus another atom type is original type") {
    assert((ExactIntegerType - InexactRationalType) === ExactIntegerType)
  }
  
  test("union type minus an atom type removes that type from the union") {
    assert((UnionType(Set(ExactIntegerType, InexactRationalType, StringType)) - ExactIntegerType) ===
      UnionType(Set(InexactRationalType, StringType))
    )
  }

  test("union type with record type minus record atom removes the record type") {
    assert((UnionType(Set(ExactIntegerType, recordType1)) - recordAtomType) ===
      ExactIntegerType
    )
  }
  
  test("union type with record atom minus record type does not remove the record atom") {
    assert((UnionType(Set(ExactIntegerType, recordAtomType)) - recordType2) ===
      UnionType(Set(ExactIntegerType, recordAtomType))
    )
  }
  
  test("atom types intersected with themselves is the original type") {
    assertIntersection(ExactIntegerType, ExactIntegerType, ExactIntegerType)
  }
  
  test("atom types intersected with another atom is an empty union") {
    assertIntersection(ExactIntegerType, InexactRationalType, UnionType(Set()))
  }

  test("atom types intersected with a union containing the atom is the original atom") {
    assertIntersection(ExactIntegerType, UnionType(Set(ExactIntegerType, StringType)), ExactIntegerType)
  }
  
  test("atom types intersected with a union not containing the atom is an empty union") {
    assertIntersection(InexactRationalType, UnionType(Set(ExactIntegerType, StringType)), UnionType(Set()))
  }

  test("distinct record types intersected with each other is an empty union") {
    assertIntersection(recordType1, recordType2, UnionType(Set()))
  }
  
  test("union of record types intersected with the record type atom is the original union") {
    assertIntersection(UnionType(Set(recordType1, recordType2)), recordAtomType, UnionType(Set(recordType1, recordType2)))
  }

  test("two union types intersected is the common member") {
    assertIntersection(UnionType(Set(InexactRationalType, ExactIntegerType)), UnionType(Set(ExactIntegerType, StringType)), ExactIntegerType)
  }

  test("scheme names for single type unions is the name of the inner type") {
    assert(UnionType(Set(ExactIntegerType)).schemeName === "<exact-integer-cell>")
  }
  
  test("scheme names for multple type unions with matching cell type") {
    assert(UnionType(Set(ExactIntegerType, InexactRationalType)).schemeName === "<numeric-cell>")
  }
  
  test("scheme names for multple type unions without matching cell type") {
    assert(UnionType(Set(ExactIntegerType, StringType)).schemeName === "(U <exact-integer-cell> <string-cell>)")
  }

  test("the cell type for the union of exact int and inexact is numeric") {
    assert(UnionType(Set(ExactIntegerType, InexactRationalType)).cellType === ct.NumericCell)
  }
  
  test("the cell type for the union of exact int, inexact and string is datum") {
    assert(UnionType(Set(ExactIntegerType, InexactRationalType, StringType)).cellType === ct.DatumCell)
  }

  test("the union of both constant booleans is the general boolean") {
    assert(SchemeType.fromTypeUnion(List(constantFalse, constantTrue)) === BooleanType)
  }

  test("general boolean type minus a constant boolean is the other constant boolean") {
    assert((BooleanType - constantTrue) === constantFalse)
    assert((BooleanType - constantFalse) === constantTrue)
  }

  test("intersection of the constant booleans in an empty union") {
    assertIntersection(constantTrue, constantFalse, UnionType(Set()))
  }

  test("intersection of the constant booleans with the general boolean is themselves") {
    assertIntersection(constantTrue, BooleanType, constantTrue)
    assertIntersection(constantFalse, BooleanType, constantFalse)
  }
  
  test("boolean constants satisfy themselvs") {
    assert(constantFalse.satisfiesType(constantFalse) === Some(true))
    assert(constantTrue.satisfiesType(constantTrue) === Some(true))
  }

  test("boolean constants satisfy the general boolean type") {
    assert(constantFalse.satisfiesType(BooleanType) === Some(true))
    assert(constantTrue.satisfiesType(BooleanType) === Some(true))
  }
}
