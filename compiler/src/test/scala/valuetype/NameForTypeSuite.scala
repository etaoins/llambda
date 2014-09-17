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
    val memberTypes = Set[NonUnionSchemeType](TopProcedureType, EmptyListType, UnitType)
    assert(NameForType(UnionType(memberTypes)) === "(U <empty-list> <procedure> <unit>)")
  }

  test("<any>") {
    assert(NameForType(AnySchemeType) === "<any>")
  }

  test("(Rec A (U <symbol> (Pairof A A)))") {
    assert(NameForType(binaryTreeType(SymbolType)) === "(Rec A (U (Pairof A A) <symbol>))")
  }

  test("(Vector <string> <symbol> <flonum>)") {
    val vectorType = SpecificVectorType(Vector[SchemeTypeRef](StringType, SymbolType, FlonumType))
    assert(NameForType(vectorType) === "(Vector <string> <symbol> <flonum>)")
  }

  test("(Vector)") {
    val vectorType = SpecificVectorType(Vector[SchemeTypeRef]())
    assert(NameForType(vectorType) === "(Vector)")
  }
  
  test("(Vectorof <number>)") {
    val vectorType = UniformVectorType(NumberType)
    assert(NameForType(vectorType) === "(Vectorof <number>)")
  }
  
  test("(Rec A (U <symbol> (Vectorof A)))") {
    val vectorType = UnionType(Set(
      SymbolType,
      UniformVectorType(RecursiveSchemeTypeRef(1))
    ))

    assert(NameForType(vectorType) === "(Rec A (U (Vectorof A) <symbol>))")
  }
  
  test("(Rec A (U <symbol> (Vector A)))") {
    val vectorType = UnionType(Set(
      SymbolType,
      SpecificVectorType(Vector(RecursiveSchemeTypeRef(1)))
    ))

    assert(NameForType(vectorType) === "(Rec A (U (Vector A) <symbol>))")
  }

  test("(-> <number>)") {
    val procedureType = ProcedureType(
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=ReturnType.SingleValue(NumberType)
    )

    assert(NameForType(procedureType) === "(-> <number>)")
  }
  
  test("(-> <string> <symbol> <number>)") {
    val procedureType = ProcedureType(
      fixedArgTypes=List(StringType, SymbolType),
      restArgMemberTypeOpt=None,
      returnType=ReturnType.SingleValue(NumberType)
    )

  assert(NameForType(procedureType) === "(-> <string> <symbol> <number>)")
  }
  
  test("(-> <string> <symbol> * <number>)") {
    val procedureType = ProcedureType(
      fixedArgTypes=List(StringType),
      restArgMemberTypeOpt=Some(SymbolType),
      returnType=ReturnType.SingleValue(NumberType)
    )

    assert(NameForType(procedureType) === "(-> <string> <symbol> * <number>)")
  }
  
  test("(-> <string> <symbol> * (values <port> <unit>)") {
    val procedureType = ProcedureType(
      fixedArgTypes=List(StringType),
      restArgMemberTypeOpt=Some(SymbolType),
      returnType=ReturnType.SpecificValues(List(PortType, UnitType))
    )

    assert(NameForType(procedureType) === "(-> <string> <symbol> * (values <port> <unit>))")
  }
  
  test("(-> <any> * *)") {
    val procedureType = ProcedureType(
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=Some(AnySchemeType),
      returnType=ReturnType.ArbitraryValues
    )

    assert(NameForType(procedureType) === "(-> <any> * *)")
  }
}
