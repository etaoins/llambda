package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

class ProcedureTypeSuite extends SchemeTypeSuite {
  val twoStringToNumberProcedure = ProcedureType(
    fixedArgTypes=List(StringType, StringType),
    restArgMemberTypeOpt=None,
    returnType=ReturnType.SingleValue(NumberType)
  )
  
  val twoStringToExactIntProcedure = ProcedureType(
    fixedArgTypes=List(StringType, StringType),
    restArgMemberTypeOpt=None,
    returnType=ReturnType.SingleValue(ExactIntegerType)
  )

  val twoStringToPortProcedure = ProcedureType(
    fixedArgTypes=List(StringType, StringType),
    restArgMemberTypeOpt=None,
    returnType=ReturnType.SingleValue(PortType)
  )
  
  val anyStringToNumberProcedure = ProcedureType(
    fixedArgTypes=Nil,
    restArgMemberTypeOpt=Some(StringType),
    returnType=ReturnType.SingleValue(NumberType)
  )

  val listElementToUnitProcedure = ProcedureType(
    fixedArgTypes=List(ListElementType),
    restArgMemberTypeOpt=None,
    returnType=ReturnType.SingleValue(UnitType)
  )
  
  val pairToUnitProcedure = ProcedureType(
    fixedArgTypes=List(AnyPairType),
    restArgMemberTypeOpt=None,
    returnType=ReturnType.SingleValue(UnitType)
  )
  
  val symbolToUnitProcedure = ProcedureType(
    fixedArgTypes=List(SymbolType),
    restArgMemberTypeOpt=None,
    returnType=ReturnType.SingleValue(UnitType)
  )
  
  val listElementsToUnitProcedure = ProcedureType(
    fixedArgTypes=Nil,
    restArgMemberTypeOpt=Some(ListElementType),
    returnType=ReturnType.SingleValue(UnitType)
  )
  
  val pairsToUnitProcedure = ProcedureType(
    fixedArgTypes=Nil,
    restArgMemberTypeOpt=Some(AnyPairType),
    returnType=ReturnType.SingleValue(UnitType)
  )
  
  val symbolsToUnitProcedure = ProcedureType(
    fixedArgTypes=Nil,
    restArgMemberTypeOpt=Some(SymbolType),
    returnType=ReturnType.SingleValue(UnitType)
  )

  test("specific procedure type satisfies itself") {
    assert(SatisfiesType(twoStringToNumberProcedure, twoStringToNumberProcedure) === Some(true))
  }

  test("procedure type does not satisfy string type") {
    assert(SatisfiesType(StringType, twoStringToPortProcedure) === Some(false))
  }

  test("string type does not satisfy procedure type") {
    assert(SatisfiesType(twoStringToPortProcedure, StringType) === Some(false))
  }
  
  test("specific procedure type definitely satisfies top procedure type") {
    assert(SatisfiesType(AnyProcedureType, twoStringToNumberProcedure) === Some(true))
  }
  
  test("any procedure type may satisfy specific procedure type") {
    assert(SatisfiesType(twoStringToNumberProcedure, AnyProcedureType) === None)
  }

  test("procedure with fixed args may satisfy procedure with compatible rest args") {
    assert(SatisfiesType(anyStringToNumberProcedure, twoStringToNumberProcedure) === None)
  }
  
  test("procedure with rest args definitely satisfies procedure with compatible fixed args") {
    assert(SatisfiesType(twoStringToNumberProcedure, anyStringToNumberProcedure) === Some(true))
  }

  test("procedure definitely satisfies procedure with more specific fixed arg type") {
    assert(SatisfiesType(pairToUnitProcedure, listElementToUnitProcedure) === Some(true))
  }
  
  test("procedure may satisfy procedure with less specific fixed arg type") {
    assert(SatisfiesType(listElementToUnitProcedure, pairToUnitProcedure) === None)
  }
  
  test("procedure definitely doesn't satisfy procedure with disjoint fixed arg type") {
    assert(SatisfiesType(listElementToUnitProcedure, symbolToUnitProcedure) === Some(false))
  }
  
  test("procedure definitely satisfies procedure with more specific rest arg type") {
    assert(SatisfiesType(pairsToUnitProcedure, listElementsToUnitProcedure) === Some(true))
  }
  
  test("procedure may satisfy procedure with less specific rest arg type") {
    assert(SatisfiesType(listElementsToUnitProcedure, pairsToUnitProcedure) === None)
  }
  
  test("procedure may satisfy procedure with disjoint rest arg type") {
    // These are compatible as long as no rest args are passed
    assert(SatisfiesType(listElementsToUnitProcedure, symbolsToUnitProcedure) === None)
  }

  test("procedure definitely satisfies procedure with less specific return type") {
    assert(SatisfiesType(twoStringToNumberProcedure, twoStringToExactIntProcedure) === Some(true))
  }
  
  test("procedure may satisfy procedure with more specific return type") {
    assert(SatisfiesType(twoStringToExactIntProcedure, twoStringToNumberProcedure) === None)
  }

  test("procedure definitely doesn't satisfy procedure with disjoint return type") {
    assert(SatisfiesType(twoStringToPortProcedure, twoStringToNumberProcedure) === Some(false))
  }
}
