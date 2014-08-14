package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}

class NameForTypeSuite extends FunSuite {
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
    val pairType = SpecificPairType(ExactIntegerType, FlonumType)
    assert(NameForType(pairType) === "(Pair <exact-integer> <flonum>)")
  }
  
  test("(Pair <any> <any>)") {
    val pairType = SpecificPairType(AnySchemeType, AnySchemeType)
    assert(NameForType(pairType) === "(Pair <any> <any>)")
  }
  
  test("(Listof <symbol>)") {
    assert(NameForType(ProperListType(SymbolType)) === "(Listof <symbol>)")
  }
  
  test("(Listof <any>)") {
    val pairType = SpecificPairType(AnySchemeType, AnySchemeType)
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
}
