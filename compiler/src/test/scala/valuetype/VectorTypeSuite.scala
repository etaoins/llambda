package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

class VectorTypeSuite extends SchemeTypeSuite {
  private val uniformNumberVector = UniformVectorType(NumberType)
  private val uniformSymbolVector = UniformVectorType(SymbolType)
  private val uniformExactIntVector = UniformVectorType(ExactIntegerType)

  private val threeNumberVector = SpecificVectorType(Vector[SchemeTypeRef](
    NumberType,
    NumberType,
    NumberType
  ))
  
  private val fiveNumberVector = SpecificVectorType(Vector[SchemeTypeRef](
    NumberType,
    NumberType,
    NumberType,
    NumberType,
    NumberType
  ))
  
  private val threeExactIntVector = SpecificVectorType(Vector[SchemeTypeRef](
    ExactIntegerType,
    ExactIntegerType,
    ExactIntegerType
  ))

  private val mixedSpecificVector = SpecificVectorType(Vector[SchemeTypeRef](
    StringType,
    SymbolType,
    ExactIntegerType
  ))

  private val emptyVector = SpecificVectorType(Vector())
  
  protected def vectorTreeType(memberType : SchemeType) : SchemeType =
    SchemeType.fromTypeUnion(List(
      memberType,
      UniformVectorType(RecursiveSchemeTypeRef(1))
    ))

  test("mixed specific vector satisfies itself") {
    assert(SatisfiesType(mixedSpecificVector, mixedSpecificVector) === Some(true))
  }
  
  test("mixed specific vector satisfies vector type atom") {
    assert(SatisfiesType(SchemeTypeAtom(ct.VectorCell), mixedSpecificVector) === Some(true))
  }
  
  test("scheme type atom may satisify mixed specific vector") {
    assert(SatisfiesType(mixedSpecificVector, SchemeTypeAtom(ct.VectorCell)) === None)
  }

  test("uniform vector may satisfy empty vector") {
    assert(SatisfiesType(emptyVector, uniformNumberVector) === None)
  }
  
  test("empty vector definitely satisfies uniform vector") {
    assert(SatisfiesType(uniformNumberVector, emptyVector) === Some(true))
  }

  test("uniform vector satifies itself") {
    assert(SatisfiesType(uniformExactIntVector, uniformExactIntVector) === Some(true))
  }
  
  test("uniform vector satifies vector type atom") {
    assert(SatisfiesType(SchemeTypeAtom(ct.VectorCell), uniformExactIntVector) === Some(true))
  }
  
  test("scheme type atom satisifies uniform vector of <any>") {
    assert(SatisfiesType(UniformVectorType(AnySchemeType), SchemeTypeAtom(ct.VectorCell)) === Some(true))
  }
  
  test("scheme type atom may satisify uniform vector of non-<any>") {
    assert(SatisfiesType(uniformSymbolVector, SchemeTypeAtom(ct.VectorCell)) === None)
  }

  test("specific vector satisfies uniform vector of same type") {
    assert(SatisfiesType(uniformNumberVector, threeNumberVector) === Some(true))
  }

  test("uniform vector may satisfy specific vector of same type") {
    assert(SatisfiesType(threeNumberVector, uniformNumberVector) === None)
  }
  
  test("specific vector satisfies uniform vector of supertype") {
    assert(SatisfiesType(uniformNumberVector, threeExactIntVector) === Some(true))
  }
  
  test("specific vector may satisfy uniform vector of subtype") {
    assert(SatisfiesType(uniformExactIntVector, threeNumberVector) === None)
  }
  
  test("specific vector does not satisify uniform vector of an unrelated type") {
    assert(SatisfiesType(uniformSymbolVector, threeNumberVector) === Some(false))
  }

  test("specific vectors of different lengths do not satisfy each other") {
    assert(SatisfiesType(fiveNumberVector, threeNumberVector) === Some(false))
  }

  test("uniform vector does not satisfy uniform vector of unrelated type") {
    assert(SatisfiesType(uniformSymbolVector, uniformExactIntVector) === Some(false))
  }

  test("uniform vector intersected with uniform vector is the most specific vector") {
    assertIntersection(uniformNumberVector, uniformExactIntVector, uniformExactIntVector) 
  }
  
  test("exact int vector tree definitely satisfies itself") {
    assert(SatisfiesType(vectorTreeType(ExactIntegerType), vectorTreeType(ExactIntegerType)) === Some(true))
  }
  
  test("exact int vector tree definitely satisfies number vector tree") {
    assert(SatisfiesType(vectorTreeType(NumberType), vectorTreeType(ExactIntegerType)) === Some(true))
  }
  
  test("number vector tree may satisfy exact int vector tree") {
    assert(SatisfiesType(vectorTreeType(ExactIntegerType), vectorTreeType(NumberType)) === None)
  }

  test("unrolling uniform vectors") {
    assert(vectorTreeType(StringType).unrolled === UnionType(Set(
      StringType,
      UniformVectorType(UnionType(Set(
        StringType,
        UniformVectorType(
          RecursiveSchemeTypeRef(1)
        )
      )))
    )))
  }
  
  test("unrolling specific vectors") {
    val binaryStringVTree = UnionType(Set(
      StringType,
      SpecificVectorType(Vector[SchemeTypeRef](
        RecursiveSchemeTypeRef(1),
        RecursiveSchemeTypeRef(1)
      ))
    ))

    assert(binaryStringVTree.unrolled === UnionType(Set(
      StringType,
      SpecificVectorType(Vector[SchemeTypeRef](
        UnionType(Set(
          StringType,
          SpecificVectorType(Vector[SchemeTypeRef](
            RecursiveSchemeTypeRef(1),
            RecursiveSchemeTypeRef(1)
          ))
        )),
        UnionType(Set(
          StringType,
          SpecificVectorType(Vector[SchemeTypeRef](
            RecursiveSchemeTypeRef(1),
            RecursiveSchemeTypeRef(1)
          ))
        ))
      ))
    )))
  }
}
