package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.dialect
import llambda.compiler.{celltype => ct}

import org.scalatest.FunSuite

import Implicits._

class StabiliseTypeSuite extends FunSuite {
  val StringToStringProcedure = ProcedureType(
    mandatoryArgTypes=List(StringType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.SingleValue(StringType)
  )

  private val stringPair = SpecificPairType(StringType, StringType)
  private val stringList = UniformProperListType(StringType)

  private val procPair = SpecificPairType(StringToStringProcedure, StringToStringProcedure)
  private val procList = UniformProperListType(StringToStringProcedure)

  test("non-applicable pair type is preserved in llambda dialect") {
    assert(StabiliseType(stringPair, dialect.Llambda) === stringPair)
  }

  test("non-applicable pair type is converted to any pair type in the R7RS dialect") {
    assert(StabiliseType(stringPair, dialect.R7RS) === AnyPairType)
  }

  test("non-applicable proper list type is preserved in llambda dialect") {
    assert(StabiliseType(stringList, dialect.Llambda) === stringList)
  }

  test("non-applicable proper list type is converted to ListElementType in R7RS dialect") {
    assert(StabiliseType(stringList, dialect.R7RS) === ListElementType)
  }

  test("applicable pair type elements are converted to top procedure type in llambda dialect") {
    assert(StabiliseType(procPair, dialect.Llambda) === SpecificPairType(
      TopProcedureType,
      TopProcedureType
    ))
  }

  test("applicable pair type is converted to any pair type in the R7RS dialect") {
    assert(StabiliseType(procPair, dialect.R7RS) === AnyPairType)
  }

  test("applicable proper list type elements are converted to top procedure type in llambda dialect") {
    assert(StabiliseType(procList, dialect.Llambda) === UniformProperListType(TopProcedureType))
  }

  test("applicable proper list type is converted to ListElementType in R7RS dialect") {
    assert(StabiliseType(procList, dialect.R7RS) === ListElementType)
  }
}
