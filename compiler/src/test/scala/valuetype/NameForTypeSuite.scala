package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

class NameForTypeSuite extends FunSuite {
  private def binaryTreeType(memberType : SchemeType) : SchemeType =
    SchemeType.fromTypeUnion(List(
      memberType,
      SpecificPairType(
        RecursiveSchemeTypeRef(1),
        RecursiveSchemeTypeRef(1)
      )
    ))

  test("<native-bool>") {
    assert(NameForType(Predicate) === "<native-bool>")
  }

  test("<native-int32>") {
    assert(NameForType(Int32) === "<native-int32>")
  }
  
  test("<native-uint8>") {
    assert(NameForType(UInt8) === "<native-uint8>")
  }
  
  test("<native-float>") {
    assert(NameForType(Float) === "<native-float>")
  }
  
  test("<native-double>") {
    assert(NameForType(Double) === "<native-double>")
  }
  
  test("<native-unicode-char>") {
    assert(NameForType(UnicodeChar) === "<native-unicode-char>")
  }
  
  test("closure type") {
    assert(NameForType(new ClosureType("procedure", Nil)) === "<internal-closure-type>")
  }
  
  test("<boolean>") {
    assert(NameForType(BooleanType) === "<boolean>")
  }
  
  test("<number>") {
    assert(NameForType(NumberType) === "<number>")
  }
  
  test("constant boolean") {
    assert(NameForType(ConstantBooleanType(true)) === "#t")
    assert(NameForType(ConstantBooleanType(false)) === "#f")
  }

  test("(Pair <exact-integer> <flonum>)") {
    val pairType = PairType(ExactIntegerType, FlonumType)
    assert(NameForType(pairType) === "(Pair <exact-integer> <flonum>)")
  }
  
  test("<pair>") {
    val pairType = PairType(AnySchemeType, AnySchemeType)
    assert(NameForType(pairType) === "<pair>")
  }
  
  test("(Listof <symbol>)") {
    assert(NameForType(ProperListType(SymbolType)) === "(Listof <symbol>)")
  }
  
  test("(Listof (Listof <symbol>))") {
    assert(NameForType(ProperListType(ProperListType(SymbolType))) === "(Listof (Listof <symbol>))")
  }
  
  test("(Listof <any>)") {
    val pairType = PairType(AnySchemeType, AnySchemeType)
    assert(NameForType(ProperListType(AnySchemeType)) === "(Listof <any>)")
  }

  test("record type") {
    assert(NameForType(new RecordType("<custom-record>", Nil)) === "<custom-record>")
  }
  
  test("empty scheme type") {
    assert(NameForType(UnionType(Set())) === "(U)")
  }

  test("single type union") {
    assert(NameForType(UnionType(Set(PortType))) === "<port>")
  }
  
  test("multiple type union") {
    val memberTypes = Set[NonUnionSchemeType](ProcedureType, EmptyListType, UnitType)
    assert(NameForType(UnionType(memberTypes)) === "(U <empty-list> <procedure> <unit>)")
  }

  test("<any>") {
    assert(NameForType(AnySchemeType) === "<any>")
  }

  test("(Rec A (U <symbol> Pair(A A)))") {
    assert(NameForType(binaryTreeType(SymbolType)) === "(Rec A (U (Pair A A) <symbol>))")
  }
}
