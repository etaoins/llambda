package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.dialect
import llambda.compiler.{celltype => ct}

import org.scalatest.FunSuite

import Implicits._

class StabiliseTypeSuite extends FunSuite {
  test("pair type is preserved in llambda dialect") {
    val stringPair = SpecificPairType(StringType, StringType)
    assert(StabiliseType(stringPair, dialect.Llambda) === stringPair)
  }

  test("pair type is converted to any pair type in the R7RS dialect") {
    val stringPair = SpecificPairType(StringType, StringType)
    assert(StabiliseType(stringPair, dialect.R7RS) === AnyPairType)
  }
  
  test("proper list type is preserved in llambda dialect") {
    val stringList = UniformProperListType(StringType)
    assert(StabiliseType(stringList, dialect.Llambda) === stringList)
  }

  test("proper list type is converted to ListElementType in R7RS dialect") {
    val stringList = UniformProperListType(StringType)
    assert(StabiliseType(stringList, dialect.R7RS) === ListElementType)
  }
}
