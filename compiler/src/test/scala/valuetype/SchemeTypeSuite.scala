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
  private val numericList = ProperListType(NumberType)
  private val exactIntList = ProperListType(ExactIntegerType)
  private val inexactList = ProperListType(FlonumType)
    
  private val knownNumberList = PairType(NumberType,
    PairType(ExactIntegerType,
      PairType(FlonumType,
        EmptyListType)))
  
  private def nonEmptyProperList(memberType : SchemeType) : SchemeType = 
    PairType(memberType, ProperListType(memberType))

  private def binaryTreeType(memberType : SchemeType) : SchemeType =
    SchemeType.fromTypeUnion(List(
      memberType,
      SpecificPairType(
        RecursiveSchemeTypeRef(1),
        RecursiveSchemeTypeRef(1)
      )
    ))
  
  private def infiniteListType(memberType : SchemeType) : SchemeType =
    SpecificPairType(
      DirectSchemeTypeRef(memberType),
      RecursiveSchemeTypeRef(0)
    )

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

  test("atom types minus themselves is an empty union") {
    assert((ExactIntegerType - ExactIntegerType) === EmptySchemeType)
  }
  
  test("atom type minus the empty type is itself") {
    assert((ExactIntegerType - EmptySchemeType) === ExactIntegerType)
  }

  test("atom type minus another atom type is original type") {
    assert((ExactIntegerType - FlonumType) === ExactIntegerType)
  }
  
  test("union type minus an atom type removes that type from the union") {
    assert((UnionType(Set(ExactIntegerType, FlonumType, StringType)) - ExactIntegerType) ===
      UnionType(Set(FlonumType, StringType))
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
  
  test("atom types intersected with another atom is the empty type") {
    assertIntersection(ExactIntegerType, FlonumType, EmptySchemeType)
  }
  
  test("atom types intersected with the empty type is the empty type") {
    assertIntersection(ExactIntegerType, EmptySchemeType, EmptySchemeType)
  }

  test("atom types intersected with a union containing the atom is the original atom") {
    assertIntersection(ExactIntegerType, UnionType(Set(ExactIntegerType, StringType)), ExactIntegerType)
  }
  
  test("atom types intersected with a union not containing the atom is an empty union") {
    assertIntersection(FlonumType, UnionType(Set(ExactIntegerType, StringType)), EmptySchemeType)
  }

  test("distinct record types intersected with each other is an empty union") {
    assertIntersection(recordType1, recordType2, EmptySchemeType)
  }
  
  test("union of record types intersected with the record type atom is the original union") {
    assertIntersection(UnionType(Set(recordType1, recordType2)), recordAtomType, UnionType(Set(recordType1, recordType2)))
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

  test("the union of both constant booleans is the general boolean") {
    assert(SchemeType.fromTypeUnion(List(constantFalse, constantTrue)) === BooleanType)
  }

  test("general boolean type minus a constant boolean is the other constant boolean") {
    assert((BooleanType - constantTrue) === constantFalse)
    assert((BooleanType - constantFalse) === constantTrue)
  }

  test("intersection of the constant booleans in an empty union") {
    assertIntersection(constantTrue, constantFalse, EmptySchemeType)
  }

  test("intersection of a constant boolean with the general boolean is itself") {
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
    val specificPairType1 = PairType(ExactIntegerType, StringType)
    val specificPairType2 = PairType(NumberType, SymbolType)

    assert(SatisfiesType(specificPairType1, specificPairType2) === Some(false))
  }

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
    assert(SatisfiesType(numericList, exactIntList) === Some(true))
  }
  
  test("proper list type may satisfy more specific proper list") {
    assert(SatisfiesType(exactIntList, numericList) === None)
  }

  test("proper list may satisfy disjoint proper list") {
    // This is because two "typed" empty lists will satisfy each other
    assert(SatisfiesType(stringList, exactIntList) === None)
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
    assert(SatisfiesType(ExactIntegerType, numericList) === Some(false))
  }
  
  test("non-list element doesn't satisfy a proper list") {
    assert(SatisfiesType(numericList, ExactIntegerType) === Some(false))
  }
  
  test("proper list type minus a compatible list is the empty union") {
    assert((exactIntList - numericList) == EmptySchemeType) 
  }
  
  test("proper list type minus the empty type is itself") {
    assert((exactIntList - EmptySchemeType) == exactIntList) 
  }
  
  test("proper list type minus an incompatible list is a pair with a proper list cdr") {
    assert((exactIntList - stringList) == PairType(ExactIntegerType, exactIntList))
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
    assert((exactIntList - PortType) == exactIntList) 
  }
  
  test("proper list type intersected with a compatible list is the most specific list") {
    assertIntersection(exactIntList, numericList, exactIntList) 
  }
  
  test("proper list type intersected with an incompatible list is an empty list") {
    assertIntersection(exactIntList, stringList, EmptyListType) 
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
    assert(SatisfiesType(nonEmptyProperList(StringType), ProperListType(StringType)) === None)
  }

  test("non-empty string proper list definitely satisfies string proper list") {
    assert(SatisfiesType(ProperListType(StringType), nonEmptyProperList(StringType)) === Some(true))
  }
  
  test("exact int binary tree definitely satisfies itself") {
    assert(SatisfiesType(binaryTreeType(ExactIntegerType), binaryTreeType(ExactIntegerType)) === Some(true))
  }
  
  test("exact int binary tree definitely satisfies number binary tree") {
    assert(SatisfiesType(binaryTreeType(NumberType), binaryTreeType(ExactIntegerType)) === Some(true))
  }
  
  test("number binary tree may satisfy exact int binary tree") {
    assert(SatisfiesType(binaryTreeType(ExactIntegerType), binaryTreeType(NumberType)) === None)
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
