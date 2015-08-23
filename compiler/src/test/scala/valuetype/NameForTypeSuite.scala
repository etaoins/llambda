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
  
  test("<boolean>") {
    assert(NameForType(BooleanType) === "<boolean>")
  }
  
  test("<number>") {
    assert(NameForType(NumberType) === "<number>")
  }

  test("constant boolean") {
    assert(NameForType(LiteralBooleanType(true)) === "#t")
    assert(NameForType(LiteralBooleanType(false)) === "#f")
  }

  test("(Pairof <exact-integer> <flonum>)") {
    val pairType = PairType(ExactIntegerType, FlonumType)
    assert(NameForType(pairType) === "(Pairof <exact-integer> <flonum>)")
  }
  
  test("<pair>") {
    val pairType = PairType(AnySchemeType, AnySchemeType)
    assert(NameForType(pairType) === "<pair>")
  }
  
  test("(Listof <symbol>)") {
    assert(NameForType(UniformProperListType(SymbolType)) === "(Listof <symbol>)")
  }
  
  test("(Listof (Listof <symbol>))") {
    assert(NameForType(
      UniformProperListType(
        UniformProperListType(
          SymbolType
        )
      )
    ) === "(Listof (Listof <symbol>))")
  }
  
  test("(Listof <any>)") {
    val pairType = PairType(AnySchemeType, AnySchemeType)
    assert(NameForType(UniformProperListType(AnySchemeType)) === "(Listof <any>)")
  }

  test("user-defined record type") {
    assert(NameForType(new RecordType("<custom-record>", Nil)) === "<custom-record>")
  }

  test("external record type") {
    assert(NameForType(new ExternalRecordType(Some("<custom-external>"), None)) === "<custom-external>")
    assert(NameForType(new ExternalRecordType(None, None)) === "<external-record-type>")
  }

  test("empty scheme type") {
    assert(NameForType(UnionType(Set())) === "(U)")
  }

  test("single type union") {
    assert(NameForType(UnionType(Set(PortType))) === "<port>")
  }
  
  test("multiple type union") {
    val memberTypes = Set[NonUnionSchemeType](TopProcedureType, EmptyListType, UnitType)
    assert(NameForType(UnionType(memberTypes)) === "(U (-> <any> * *) <empty-list> <unit>)")
  }

  test("<any>") {
    assert(NameForType(AnySchemeType) === "<any>")
  }

  test("(Rec A (U <symbol> (Pairof A A)))") {
    assert(NameForType(binaryTreeType(SymbolType)) === "(Rec A (U (Pairof A A) <symbol>))")
  }

  test("(-> <number>)") {
    val procedureType = ProcedureType(
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=ReturnType.SingleValue(NumberType)
    )

    assert(NameForType(procedureType) === "(-> <number>)")
  }

  test("(-> <string> <symbol> <number>)") {
    val procedureType = ProcedureType(
      mandatoryArgTypes=List(StringType, SymbolType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=ReturnType.SingleValue(NumberType)
    )

  assert(NameForType(procedureType) === "(-> <string> <symbol> <number>)")
  }

  test("(-> <string> <symbol> * <number>)") {
    val procedureType = ProcedureType(
      mandatoryArgTypes=List(StringType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(SymbolType),
      returnType=ReturnType.SingleValue(NumberType)
    )

    assert(NameForType(procedureType) === "(-> <string> <symbol> * <number>)")
  }

  test("(-> <string> <symbol> * (values <port> <unit>)") {
    val procedureType = ProcedureType(
      mandatoryArgTypes=List(StringType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(SymbolType),
      returnType=ReturnType.SpecificValues(List(PortType, UnitType))
    )

    assert(NameForType(procedureType) === "(-> <string> <symbol> * (values <port> <unit>))")
  }

  test("(-> <any> * *)") {
    val procedureType = ProcedureType(
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(AnySchemeType),
      returnType=ReturnType.ArbitraryValues
    )

    assert(NameForType(procedureType) === "(-> <any> * *)")
  }

  test("(-> <any> <unit> (unreachable))") {
    val procedureType = ProcedureType(
      mandatoryArgTypes=List(AnySchemeType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=ReturnType.UnreachableValue
    )

    assert(NameForType(procedureType) === "(-> <any> <unit> (unreachable))")
  }

  test("(->* (<string>) (<symbol>) <number>)") {
    val procedureType = ProcedureType(
      mandatoryArgTypes=List(StringType),
      optionalArgTypes=List(SymbolType),
      restArgMemberTypeOpt=None,
      returnType=ReturnType.SingleValue(NumberType)
    )

    assert(NameForType(procedureType) === "(->* (<string>) (<symbol>) <number>)")
  }

  test("(->* () (<boolean> <symbol>) <port> * <number>)") {
    val procedureType = ProcedureType(
      mandatoryArgTypes=Nil,
      optionalArgTypes=List(BooleanType, SymbolType),
      restArgMemberTypeOpt=Some(PortType),
      returnType=ReturnType.SingleValue(NumberType)
    )

    assert(NameForType(procedureType) === "(->* () (<boolean> <symbol>) <port> * <number>)")
  }

  test("(case-> (-> <number>) (-> <number> <number>))") {
    val caseProcType = CaseProcedureType(List(
      ProcedureType(
        mandatoryArgTypes=Nil,
        optionalArgTypes=Nil,
        restArgMemberTypeOpt=None,
        returnType=ReturnType.SingleValue(NumberType)
      ),
      ProcedureType(
        mandatoryArgTypes=List(NumberType),
        optionalArgTypes=Nil,
        restArgMemberTypeOpt=None,
        returnType=ReturnType.SingleValue(NumberType)
      )
    ))

    assert(NameForType(caseProcType) === "(case-> (-> <number>) (-> <number> <number>))")
  }

  test("'hello") {
    assert(NameForType(LiteralSymbolType("hello")) === "'hello")
  }

  test("'|Hello, world!|") {
    assert(NameForType(LiteralSymbolType("Hello, world!")) === "'|Hello, world!|")
  }

  test("(Rec A (U (Pairof A A) <empty-list>))") {
    val recursiveProperList = UnionType(Set(
      SpecificPairType(RecursiveSchemeTypeRef(1), RecursiveSchemeTypeRef(1)),
      EmptyListType
    ))

    // Make sure this doesn't infinitely recurse in the proper list special casing code
    assert(NameForType(recursiveProperList) === "(Rec A (U (Pairof A A) <empty-list>))")
  }

  test("(HashMap <any> <any>)") {
    assert(NameForType(AnyHashMapType) === "(HashMap <any> <any>)")
  }

  test("(HashMap <pair> <unit>)") {
    assert(NameForType(HashMapType(AnyPairType, UnitType)) === "(HashMap <pair> <unit>)")
  }
}
