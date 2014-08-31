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
    val stringList = ProperListType(StringType)
    assert(StabiliseType(stringList, dialect.Llambda) === stringList)
  }

  test("proper list type is converted to ListElementType in R7RS dialect") {
    val stringList = ProperListType(StringType)
    assert(StabiliseType(stringList, dialect.R7RS) === ListElementType)
  }

  test("uniform vector type is stabilised to vector type atom") {
    val stringVector = UniformVectorType(StringType)
    assert(StabiliseType(stringVector, dialect.Llambda) === SchemeTypeAtom(ct.VectorCell))
  }

  test("specific vector type is stabilised to same length vector of <any>") {
    val threeStringVector = SpecificVectorType(Vector[SchemeTypeRef](
      StringType,
      StringType,
      StringType
    ))
    
    val threeAnyVector = SpecificVectorType(Vector[SchemeTypeRef](
      AnySchemeType,
      AnySchemeType,
      AnySchemeType
    ))

    assert(StabiliseType(threeStringVector, dialect.Llambda) === threeAnyVector)
  }
}
