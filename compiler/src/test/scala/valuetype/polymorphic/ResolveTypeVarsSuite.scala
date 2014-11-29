package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import io.llambda.compiler.valuetype._
import Implicits._

class ResolveTypeVarsSuite extends FunSuite {
  val polyA = new TypeVar("A")
  val polyB = new TypeVar("B")

  test("resolving leaf types") {
    val polyLeaf = ExactIntegerType
    val evidence = ExactIntegerType

    assert(ResolveTypeVars(Set(), polyLeaf, evidence).values == Map())
  }

  test("resolving a type var directly") {
    val polyVar = polyA
    val evidence = ExactIntegerType

    assert(ResolveTypeVars(Set(polyA), polyVar, evidence).values == Map(
      polyA -> ExactIntegerType
    ))
  }

  test("resolving simple pair type with distinct car and cdr variables") {
    val polyPair = SpecificPairType(polyA, polyB)

    val evidence = SpecificPairType(
      ExactIntegerType,
      FlonumType
    )

    assert(ResolveTypeVars(Set(polyA, polyB), polyPair, evidence).values == Map(
      polyA -> ExactIntegerType,
      polyB -> FlonumType
    ))
  }

  test("resolving a pair with empty scheme type") {
    val polyPair = SpecificPairType(polyA, polyB)
    val evidence = EmptySchemeType

    assert(ResolveTypeVars(Set(polyA), polyPair, evidence).values == Map())
  }


  test("resolving simple pair type with same car and cdr variables") {
    val polyPair = SpecificPairType(polyA, polyA)

    val evidence = SpecificPairType(
      ExactIntegerType,
      FlonumType
    )

    assert(ResolveTypeVars(Set(polyA), polyPair, evidence).values == Map(
      polyA -> NumberType
    ))
  }

  test("resolving uniform proper list type with uniform proper list") {
    val polyList = UnionType(Set(
      EmptyListType,
      SpecificPairType(
        polyA,
        RecursiveSchemeTypeRef(1)  // Inner recursive var
      )
    ))

    val evidence = UniformProperListType(ExactIntegerType)

    assert(ResolveTypeVars(Set(polyA), polyList, evidence).values == Map(
      polyA -> ExactIntegerType
    ))
  }

  test("resolving uniform proper list type with specific proper list") {
    val polyList = UnionType(Set(
      EmptyListType,
      SpecificPairType(
        polyA,
        RecursiveSchemeTypeRef(1)  // Inner recursive var
      )
    ))

    val evidence = SpecificProperListType(Vector(ExactIntegerType, FlonumType, ExactIntegerType))

    assert(ResolveTypeVars(Set(polyA), polyList, evidence).values == Map(
      polyA -> NumberType
    ))
  }

  test("resolving specific proper list type with uniform proper list") {
    val polyList =
      SpecificPairType(
        polyA,
        SpecificPairType(
          polyA,
          SpecificPairType(
            polyA,
            EmptyListType
          )
        )
      )

    val evidence = UniformProperListType(PortType)

    assert(ResolveTypeVars(Set(polyA), polyList, evidence).values == Map(
      polyA -> PortType
    ))
  }

  test("resolving uniform vector type with uniform vector") {
    val polyVec = UniformVectorType(polyA)
    val evidence = UniformVectorType(ExactIntegerType)

    assert(ResolveTypeVars(Set(polyA), polyVec, evidence).values == Map(
      polyA -> ExactIntegerType
    ))
  }

  test("resolving specific vector type with specific vector") {
    val polyVec = SpecificVectorType(Vector(polyA, polyB, polyA))

    val evidence = SpecificVectorType(Vector(
      ExactIntegerType,
      PortType,
      FlonumType
    ))

    assert(ResolveTypeVars(Set(polyA, polyB), polyVec, evidence).values == Map(
      polyA -> NumberType,
      polyB -> PortType
    ))
  }

  test("resolving uniform vector type with specific vector") {
    val polyVec = UniformVectorType(polyA)

    val evidence = SpecificVectorType(Vector(
      ExactIntegerType,
      FlonumType
    ))

    assert(ResolveTypeVars(Set(polyA), polyVec, evidence).values == Map(
      polyA -> NumberType
    ))
  }
}
