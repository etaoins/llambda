package io.llambda.compiler.valuetype

import org.scalatest.FunSuite

import Implicits._


class StabiliseTypeSuite extends FunSuite {
  val StringToStringProcedure = ProcedureType(
    mandatoryArgTypes=List(StringType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(StringType)
  )

  private val stringPair = SpecificPairType(StringType, StringType)
  private val stringList = UniformProperListType(StringType)

  private val procPair = SpecificPairType(StringToStringProcedure, StringToStringProcedure)
  private val procList = UniformProperListType(StringToStringProcedure)

  test("non-procedure pair type is preserved") {
    assert(StabiliseType(stringPair) === stringPair)
  }

  test("non-procedure proper list type is preserved") {
    assert(StabiliseType(stringList) === stringList)
  }

  test("procedure pair type elements are converted to top procedure type") {
    assert(StabiliseType(procPair) === SpecificPairType(
      TopProcedureType,
      TopProcedureType
    ))
  }

  test("procedure proper list type elements are converted to top procedure type") {
    assert(StabiliseType(procList) === UniformProperListType(TopProcedureType))
  }
}
