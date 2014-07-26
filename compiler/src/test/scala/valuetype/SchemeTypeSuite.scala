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

  private val stringList = ProperListType(StringType)
  private val numericList = ProperListType(NumericType)
  private val exactIntList = ProperListType(ExactIntegerType)
  private val inexactList = ProperListType(InexactRationalType)
    
  private val knownNumberList = PairType(NumericType,
    PairType(ExactIntegerType,
      PairType(InexactRationalType,
        EmptyListType)))


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

  test("atom types satisfy themselves") {
    assert(SatisfiesType(ExactIntegerType, ExactIntegerType) ===
      Some(true)
    )
  }
  
  test("atom types definitely don't satisfy other atom types") {
    assert(SatisfiesType(ExactIntegerType, InexactRationalType) ===
      Some(false)
    )
  }

  test("record types definitely satisfy themselves") {
    assert(SatisfiesType(recordType1, recordType1) ===
      Some(true)
    )
  }
  
  test("record types definitely don't satisfy other record types") {
    assert(SatisfiesType(recordType1, recordType2) ===
      Some(false)
    )
  }
  
  test("record types definitely satisfy the record atom") {
    assert(SatisfiesType(recordAtomType, recordType1) ===
      Some(true)
    )
  }

  test("the record atom may satisfy a record type") {
    assert(SatisfiesType(recordType2, recordAtomType) ===
      None
    )
  }

  test("atom types definitely satisfy a union containing themselves") {
    val unionWithExactInt = UnionType(Set(ExactIntegerType, InexactRationalType))

    assert(SatisfiesType(unionWithExactInt, ExactIntegerType) ===
      Some(true)
    )
  }
  
  test("atom types definitely don't satisfy a union not containing themselves") {
    val unionWithoutExactInt = UnionType(Set(InexactRationalType, InexactRationalType))

    assert(SatisfiesType(unionWithoutExactInt, ExactIntegerType) ===
      Some(false)
    )
  }

  test("union types definitely satisfy superset unions") {
    val subunion = UnionType(Set(InexactRationalType, ExactIntegerType))
    val superunion = UnionType(Set(StringType, InexactRationalType, ExactIntegerType))

    assert(SatisfiesType(superunion, subunion) === Some(true))
  }
  
  test("union types definitely don't satisfy disjoint unions") {
    val union1 = UnionType(Set(InexactRationalType, ExactIntegerType))
    val union2 = UnionType(Set(StringType))

    assert(SatisfiesType(union1, union2) === Some(false))
    assert(SatisfiesType(union2, union1) === Some(false))
  }
  
  test("union types may satisfy intersecting union type") {
    val union1 = UnionType(Set(InexactRationalType, ExactIntegerType))
    val union2 = UnionType(Set(StringType, InexactRationalType))

    assert(SatisfiesType(union1, union2) === None)
    assert(SatisfiesType(union2, union1) === None)
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
    assert(SatisfiesType(constantFalse, constantFalse) === Some(true))
    assert(SatisfiesType(constantTrue, constantTrue) === Some(true))
  }

  test("boolean constants satisfy the general boolean type") {
    assert(SatisfiesType(BooleanType, constantFalse) === Some(true))
    assert(SatisfiesType(BooleanType, constantTrue) === Some(true))
  }

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

  test("incompatible specific pair types do not satisfy each other") {
    val specificPairType1 = PairType(SymbolType, StringType)
    val specificPairType2 = PairType(StringType, SymbolType)

    assert(SatisfiesType(specificPairType1, specificPairType2) === Some(false))
  }

  test("proper list type satisfies itself") {
    assert(SatisfiesType(stringList, stringList) === Some(true))
  }
  
  test("proper list type satisfies list element type") {
    assert(SatisfiesType(ListElementType, stringList) === Some(true))
  }
  
  test("list element type may satisfy proper list type") {
    assert(SatisfiesType(stringList, ListElementType) === None)
  }
  
  test("proper list type satisfies more general proper list") {
    assert(SatisfiesType(numericList, exactIntList) === Some(true))
  }
  
  test("proper list type may satisfy more specific proper list") {
    assert(SatisfiesType(exactIntList, numericList) === None)
  }

  test("proper list does not satisfy disjoint proper list") {
    assert(SatisfiesType(stringList, exactIntList) === Some(false))
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
    assert(SatisfiesType(knownNumberList, stringList) === None)
  }
  
  test("proper list type definitely doesn't satisfy non-list element") {
    assert(SatisfiesType(ExactIntegerType, numericList) === Some(false))
  }
  
  test("non-list element doesn't satisfy a proper list") {
    assert(SatisfiesType(numericList, ExactIntegerType) === Some(false))
  }
  
  test("proper list type minus a compatible list is the empty union") {
    assert((exactIntList - numericList) == UnionType(Set())) 
  }
  
  test("proper list type minus an incompatible list is itself") {
    assert((exactIntList - stringList) == exactIntList) 
  }

  test("proper list type minus the empty list type is its pair type") {
    assert((stringList - EmptyListType) == SpecificPairType(StringType, stringList)) 
  }
  
  test("proper list type minus a compatible pair type is the empty list") {
    assert((stringList - AnyPairType) == EmptyListType) 
  }
  
  test("proper list type minus an incompatible pair type is itself") {
    assert((stringList - SpecificPairType(SymbolType, StringType)) == stringList) 
  }
  
  test("proper list type minus the list element type is the empty union") {
    assert((stringList - ListElementType) == UnionType(Set()))
  }
  
  test("the list element type minus a proper list is a pair") {
    // EmptyListType is a proper list so this must be a pair
    assert((ListElementType - stringList) == AnyPairType)
  }
  
  test("proper list type minus an unrelated type is itself") {
    assert((exactIntList - PortType) == exactIntList) 
  }
  
  test("proper list type intersected with a compatible list is the most specific list") {
    assertIntersection(exactIntList, numericList, exactIntList) 
  }
  
  test("proper list type intersected with an incompatible list is the empty union") {
    assertIntersection(exactIntList, stringList, UnionType(Set())) 
  }

  test("proper list type intersected with the empty list is an empty list") {
    assertIntersection(stringList, EmptyListType, EmptyListType) 
  }
  
  test("proper list type intersected with a compatible pair is its pair type") {
    assertIntersection(stringList, AnyPairType, SpecificPairType(StringType, stringList)) 
  }
  
  test("proper list type intersected with an incompatible pair is an empty union") {
    assertIntersection(stringList, SpecificPairType(SymbolType, StringType), UnionType(Set()))
  }
  
  test("proper list type intersected with the list element type is itself") {
    assertIntersection(stringList, ListElementType, stringList)
  }
  
  test("proper list type intersected with an unrelated type is an empty union") {
    assertIntersection(stringList, PortType, UnionType(Set()))
  }
}
