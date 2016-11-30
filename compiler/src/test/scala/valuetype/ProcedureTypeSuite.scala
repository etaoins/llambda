package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

class ProcedureTypeSuite extends SchemeTypeSuite {
  val twoStringToNumberProcedure = ProcedureType(
    mandatoryArgTypes=List(StringType, StringType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(NumberType)
  )

  val twoStringToIntProcedure = ProcedureType(
    mandatoryArgTypes=List(StringType, StringType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(IntegerType)
  )

  val twoStringToPortProcedure = ProcedureType(
    mandatoryArgTypes=List(StringType, StringType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(PortType)
  )

  val anyStringToNumberProcedure = ProcedureType(
    mandatoryArgTypes=Nil,
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=Some(StringType),
    returnType=ReturnType.Reachable(NumberType)
  )

  val listElementToUnitProcedure = ProcedureType(
    mandatoryArgTypes=List(ListElementType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(UnitType)
  )

  val pairToUnitProcedure = ProcedureType(
    mandatoryArgTypes=List(AnyPairType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(UnitType)
  )

  val symbolToUnreachableProcedure = ProcedureType(
    mandatoryArgTypes=List(SymbolType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Unreachable
  )

  val symbolToStringProcedure = ProcedureType(
    mandatoryArgTypes=List(SymbolType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(StringType)
  )

  val symbolToUnitProcedure = ProcedureType(
    mandatoryArgTypes=List(SymbolType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(UnitType)
  )

  val listElementsToUnitProcedure = ProcedureType(
    mandatoryArgTypes=Nil,
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=Some(ListElementType),
    returnType=ReturnType.Reachable(UnitType)
  )

  val pairsToUnitProcedure = ProcedureType(
    mandatoryArgTypes=Nil,
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=Some(AnyPairType),
    returnType=ReturnType.Reachable(UnitType)
  )

  val symbolsToUnitProcedure = ProcedureType(
    mandatoryArgTypes=Nil,
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=Some(SymbolType),
    returnType=ReturnType.Reachable(UnitType)
  )

  val higherOrderProcedure = ProcedureType(
    mandatoryArgTypes=List(symbolToUnitProcedure, anyStringToNumberProcedure),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=Some(listElementToUnitProcedure),
    returnType=ReturnType.Reachable(twoStringToIntProcedure)
  )

  val optionalStringToNumberProcedure = ProcedureType(
    mandatoryArgTypes=List(StringType),
    optionalArgTypes=List(StringType),
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(NumberType)
  )

  val oneStringToNumberProcedure = ProcedureType(
    mandatoryArgTypes=List(StringType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(NumberType)
  )

  test("specific procedure type satisfies itself") {
    assert(SatisfiesType(twoStringToNumberProcedure, twoStringToNumberProcedure) === Some(true))
  }

  test("specific procedure type's applicable type is itself") {
    assert(twoStringToNumberProcedure.applicableTypeOpt === Some(twoStringToNumberProcedure))
  }

  test("higher order procedure type satisfies itself") {
    assert(SatisfiesType(higherOrderProcedure, higherOrderProcedure) === Some(true))
  }

  test("procedure type does not satisfy string type") {
    assert(SatisfiesType(StringType, twoStringToPortProcedure) === Some(false))
  }

  test("string type does not satisfy procedure type") {
    assert(SatisfiesType(twoStringToPortProcedure, StringType) === Some(false))
  }

  test("procedure type is convertible to the unit type") {
    assert(ConvertibleToType(UnitType, StringType) === Some(true))
  }

  test("specific procedure type may satisfy top procedure type") {
    assert(SatisfiesType(TopProcedureType, twoStringToNumberProcedure) === None)
  }

  test("specific procedure type definitely satisfies procedure atom type") {
    assert(SatisfiesType(SchemeTypeAtom(ct.ProcedureCell), twoStringToNumberProcedure) === Some(true))
  }

  test("specific procedure type definitely satisfies <any> type") {
    assert(SatisfiesType(AnySchemeType, twoStringToNumberProcedure) === Some(true))
  }

  test("higher order procedure type may satisfy top procedure type") {
    assert(SatisfiesType(TopProcedureType, higherOrderProcedure) === None)
  }

  test("higher order procedure type definitely satisfies procedure type atom") {
    assert(SatisfiesType(SchemeTypeAtom(ct.ProcedureCell), higherOrderProcedure) === Some(true))
  }

  test("higher order procedure type definitely satisfies <any> type") {
    assert(SatisfiesType(AnySchemeType, higherOrderProcedure) === Some(true))
  }

  test("top procedure type may satisfy specific procedure type") {
    assert(SatisfiesType(twoStringToNumberProcedure, TopProcedureType) === None)
  }

  test("top procedure type's applicable type is itself") {
    assert(TopProcedureType.applicableTypeOpt === Some(TopProcedureType))
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
    assert(SatisfiesType(twoStringToNumberProcedure, twoStringToIntProcedure) === Some(true))
  }

  test("procedure may satisfy procedure with more specific return type") {
    assert(SatisfiesType(twoStringToIntProcedure, twoStringToNumberProcedure) === None)
  }

  test("procedure definitely doesn't satisfy procedure with disjoint return type") {
    assert(SatisfiesType(twoStringToPortProcedure, twoStringToNumberProcedure) === Some(false))
  }

  test("procedure returning a value satisfies procedure returning unit") {
    assert(SatisfiesType(symbolToUnitProcedure, symbolToStringProcedure) === Some(true))
  }

  test("procedure returning unit does not satisfy procedure returning a value") {
    assert(SatisfiesType(symbolToStringProcedure, symbolToUnitProcedure) === Some(false))
  }

  test("procedure with unreachable return satisfies procedure returning string") {
    assert(SatisfiesType(symbolToStringProcedure, symbolToUnreachableProcedure) === Some(true))
  }

  test("the union of two identical specific procedure types is that procedure type") {
    assert((twoStringToPortProcedure + twoStringToPortProcedure) === twoStringToPortProcedure)
  }

  test("procedure with optional string definitely satisfies procedure with mandatory string argument") {
    assert(SatisfiesType(twoStringToNumberProcedure, optionalStringToNumberProcedure) === Some(true))
  }

  test("procedure with mandatory string definitely satisfies procedure with optional string argument") {
    assert(SatisfiesType(optionalStringToNumberProcedure, twoStringToNumberProcedure) === None)
  }

  test("procedure with optional string may satisfy procedure without corresponding argument") {
    assert(SatisfiesType(oneStringToNumberProcedure, optionalStringToNumberProcedure) === Some(true))
  }

  test("procedure without corresponding arguemnt may satisfy procedure with optional string argument") {
    assert(SatisfiesType(optionalStringToNumberProcedure, oneStringToNumberProcedure) === None)
  }

  test("the union of two unrelated specific procedure types is the top procedure type") {
    assert((twoStringToPortProcedure + anyStringToNumberProcedure) === TopProcedureType)
  }

  test("the union of two related specific procedure types is the most general procedure type") {
    assert((twoStringToNumberProcedure + twoStringToIntProcedure) === twoStringToNumberProcedure)
  }

  test("the union of a specific procedure type and the top procedure type is the top procedure type") {
    assert((twoStringToPortProcedure + TopProcedureType) === TopProcedureType)
  }

  test("the union of a specific procedure type and the string type has the applicable type of the specific procedure type") {
    assert((twoStringToPortProcedure + StringType).applicableTypeOpt === Some(twoStringToPortProcedure))
  }

  test("the union of two non-procedure types has no procedure type") {
    assert((NumberType + StringType).applicableTypeOpt === None)
  }

  test("the procedure type atom has the applicable type of the top procedure type") {
    assert(SchemeTypeAtom(ct.ProcedureCell).applicableTypeOpt === Some(TopProcedureType))
  }

  test("the <any> type has a applicable type of the top procedure type") {
    assert(AnySchemeType.applicableTypeOpt === Some(TopProcedureType))
  }

  test("replacing the applicable type of a non-procedure type") {
    assert(NumberType.replaceApplicableType(listElementToUnitProcedure) === NumberType)
  }

  test("replacing the applicable type of the procedure type atom") {
    val replacedType = SchemeTypeAtom(ct.ProcedureCell).replaceApplicableType(listElementToUnitProcedure)
    assert(replacedType === listElementToUnitProcedure)
  }

  test("replacing the applicable type of a procedure type") {
    val replacedType = twoStringToPortProcedure.replaceApplicableType(listElementToUnitProcedure)
    assert(replacedType === listElementToUnitProcedure)
  }

  test("replacing the applicable type of a type union") {
    val replacedType = (twoStringToPortProcedure + NumberType).replaceApplicableType(listElementToUnitProcedure)
    assert(replacedType === (listElementToUnitProcedure + NumberType))
  }
}
