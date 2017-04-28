package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}


class CaseProcedureTypeSuite extends SchemeTypeSuite {
  val oneUnitToPortProcedure = ProcedureType(
    mandatoryArgTypes=List(UnitType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(PortType)
  )

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

  val threeSymbolToNullProcedure = ProcedureType(
    mandatoryArgTypes=List(SymbolType, SymbolType, SymbolType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(ListElementType)
  )

  val fourPortToNullProcedure = ProcedureType(
    mandatoryArgTypes=List(PortType, PortType, PortType, PortType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(ListElementType)
  )

  test("case procedure definitely satisfies itself") {
    val superCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    val testingCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    assert(SatisfiesType(superCase, testingCase) === Some(true))
  }

  test("case procedure definitely satisfies case with more clauses") {
    val superCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure, threeSymbolToNullProcedure)
    )

    val testingCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    assert(SatisfiesType(superCase, testingCase) === Some(true))
  }

  test("case procedure may satisfy case with fewer clauses") {
    val superCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    val testingCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure, threeSymbolToNullProcedure)
    )

    assert(SatisfiesType(superCase, testingCase) === None)
  }

  test("case procedure may satisfy case with partially disjoint clauses") {
    val superCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    val testingCase = CaseProcedureType(
      List(oneUnitToPortProcedure, threeSymbolToNullProcedure)
    )

    assert(SatisfiesType(superCase, testingCase) === None)
  }

  test("case procedure definitely satisfies case with super clauses") {
    val superCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    val testingCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToIntProcedure)
    )

    assert(SatisfiesType(superCase, testingCase) === Some(true))
  }

  test("case procedure may satisfy case with sub clauses") {
    val superCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToIntProcedure)
    )

    val testingCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    assert(SatisfiesType(superCase, testingCase) === None)
  }

  test("case procedure definitely does not satisfy cases with disjoint clauses") {
    val superCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    val testingCase = CaseProcedureType(
      List(threeSymbolToNullProcedure, fourPortToNullProcedure)
    )

    assert(SatisfiesType(superCase, testingCase) === Some(false))
  }

  test("case procedure definitely may satisfy top procedure type") {
    val testingCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    assert(SatisfiesType(TopProcedureType, testingCase) === None)
  }

  test("case procedure definitely satifies procedure type atom") {
    val testingCase = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    assert(SatisfiesType(SchemeTypeAtom(ct.ProcedureCell), testingCase) === Some(true))
  }

  test("case procedure's applicable type is itself") {
    val caseProcType = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    assert(caseProcType.applicableTypeOpt === Some(caseProcType))
  }

  test("the union of a case procedure and a clause procedure type is the case procedure type") {
    val caseProcType = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    assert((caseProcType + oneUnitToPortProcedure) === caseProcType)
  }

  test("the union of a case procedure and a disjoint procedure type is case procedure type") {
    val caseProcType = CaseProcedureType(
      List(oneUnitToPortProcedure, twoStringToNumberProcedure)
    )

    assert((caseProcType + threeSymbolToNullProcedure) === TopProcedureType)
  }
}
