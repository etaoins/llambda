package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import io.llambda.compiler.valuetype._
import Implicits._

class ResolveTypeVarsSuite extends FunSuite {
  test("resolving leaf types") {
    val polyLeaf = ExactIntegerType
    val evidence = ExactIntegerType

    assert(ResolveTypeVars(polyLeaf, evidence).values == Map())
  }

  test("resolving simple pair type with distinct car and cdr variables") {
    val polyPair = SpecificPairType(
      RecursiveSchemeTypeRef(1),
      RecursiveSchemeTypeRef(2)
    )

    val evidence = SpecificPairType(
      ExactIntegerType,
      FlonumType
    )

    assert(ResolveTypeVars(polyPair, evidence).values == Map(
      0 -> ExactIntegerType,
      1 -> FlonumType
    ))
  }

  test("resolving simple pair type with same car and cdr variables") {
    val polyPair = SpecificPairType(
      RecursiveSchemeTypeRef(1),
      RecursiveSchemeTypeRef(1)
    )

    val evidence = SpecificPairType(
      ExactIntegerType,
      FlonumType
    )

    assert(ResolveTypeVars(polyPair, evidence).values == Map(
      0 -> NumberType
    ))
  }

  test("resolving uniform proper list type with uniform proper list") {
    val polyList = UnionType(Set(
      EmptyListType,
      SpecificPairType(
        RecursiveSchemeTypeRef(2), // Polymorphic var
        RecursiveSchemeTypeRef(1)  // Inner recursive var
      )
    ))

    val evidence = UniformProperListType(ExactIntegerType)

    assert(ResolveTypeVars(polyList, evidence).values == Map(
      0 -> ExactIntegerType
    ))
  }

  test("resolving uniform proper list type with specific proper list") {
    val polyList = UnionType(Set(
      EmptyListType,
      SpecificPairType(
        RecursiveSchemeTypeRef(2), // Poly A
        RecursiveSchemeTypeRef(1)  // Inner recursive var
      )
    ))

    val evidence = SpecificProperListType(Vector(ExactIntegerType, FlonumType, ExactIntegerType))

    assert(ResolveTypeVars(polyList, evidence).values == Map(
      0 -> NumberType
    ))
  }

  test("resolving specific proper list type with uniform proper list") {
    val polyList =
      SpecificPairType(
        RecursiveSchemeTypeRef(1), // Poly A
        SpecificPairType(
          RecursiveSchemeTypeRef(2), // Poly A
          SpecificPairType(
            RecursiveSchemeTypeRef(3), // Poly A
            EmptyListType
          )
        )
      )

    val evidence = UniformProperListType(PortType)

    assert(ResolveTypeVars(polyList, evidence).values == Map(
      0 -> PortType
    ))
  }

  test("resolving uniform vector type with uniform vector") {
    val polyVec = UniformVectorType(RecursiveSchemeTypeRef(1))
    val evidence = UniformVectorType(ExactIntegerType)

    assert(ResolveTypeVars(polyVec, evidence).values == Map(
      0 -> ExactIntegerType
    ))
  }

  test("resolving specific vector type with specific vector") {
    val polyVec = SpecificVectorType(Vector(
      RecursiveSchemeTypeRef(1), // Poly A
      RecursiveSchemeTypeRef(2), // Poly B
      RecursiveSchemeTypeRef(1)  // Poly A
    ))

    val evidence = SpecificVectorType(Vector(
      ExactIntegerType,
      PortType,
      FlonumType
    ))

    assert(ResolveTypeVars(polyVec, evidence).values == Map(
      0 -> NumberType,
      1 -> PortType
    ))
  }

  test("resolving uniform vector type with specific vector") {
    val polyVec = UniformVectorType(RecursiveSchemeTypeRef(1))

    val evidence = SpecificVectorType(Vector(
      ExactIntegerType,
      FlonumType
    ))

    assert(ResolveTypeVars(polyVec, evidence).values == Map(
      0 -> NumberType
    ))
  }
}
