package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import io.llambda.compiler.valuetype._
import io.llambda.compiler.NoSourceLocation
import io.llambda.compiler.ImpossibleTypeConversionException

import Implicits._

class InstantiateTypeSuite extends FunSuite {
  test("instantiating a native type") {
    val polyNative = Int64
    val reconciled = ReconcileTypeVars.Result(Vector())
    val expected = Int64

    assert(InstantiateType(polyNative, reconciled) === expected)
  }

  test("instantiating a leaf Scheme type") {
    val polyLeaf = ExactIntegerType
    val reconciled = ReconcileTypeVars.Result(Vector())
    val expected = ExactIntegerType

    assert(InstantiateType(polyLeaf, reconciled) === expected)
  }

  test("instantiating a pair type") {
    val polyPair = SpecificPairType(
      RecursiveSchemeTypeRef(1), // Poly A
      RecursiveSchemeTypeRef(2)  // Poly B
    )

    val reconciled = ReconcileTypeVars.Result(Vector(
      ExactIntegerType,
      FlonumType
    ))

    val expected = SpecificPairType(
      ExactIntegerType,
      FlonumType
    )

    assert(InstantiateType(polyPair, reconciled) == expected)
  }

  test("instantiating a nested pair") {
    val polyPair = SpecificPairType(
      RecursiveSchemeTypeRef(1), // Poly A
      SpecificPairType(
        RecursiveSchemeTypeRef(2), // Poly A
        RecursiveSchemeTypeRef(2)  // Poly A
      )
    )

    val reconciled = ReconcileTypeVars.Result(Vector(
      ExactIntegerType
    ))

    val expected = SpecificPairType(
      ExactIntegerType,
      SpecificPairType(
        ExactIntegerType,
        ExactIntegerType
      )
    )

    assert(InstantiateType(polyPair, reconciled) == expected)
  }

  test("instantiating a uniform vector") {
    val polyVec = UniformVectorType(
      RecursiveSchemeTypeRef(1) // Poly A
    )

    val reconciled = ReconcileTypeVars.Result(Vector(
      ExactIntegerType
    ))

    val expected = UniformVectorType(
      ExactIntegerType
    )

    assert(InstantiateType(polyVec, reconciled) == expected)
  }

  test("instantiating a specific vector") {
    val polyVec = SpecificVectorType(Vector(
      RecursiveSchemeTypeRef(1),
      RecursiveSchemeTypeRef(2),
      RecursiveSchemeTypeRef(1)
    ))

    val reconciled = ReconcileTypeVars.Result(Vector(
      ExactIntegerType,
      FlonumType
    ))

    val expected = SpecificVectorType(Vector(
      ExactIntegerType,
      FlonumType,
      ExactIntegerType
    ))

    assert(InstantiateType(polyVec, reconciled) == expected)
  }

  test("instantiating a uniform vector of pairs") {
    val polyVec = UniformVectorType(
      SpecificPairType(
        RecursiveSchemeTypeRef(3), // Poly B
        RecursiveSchemeTypeRef(2)  // Poly A
      )
    )

    val reconciled = ReconcileTypeVars.Result(Vector(
      ExactIntegerType,
      FlonumType
    ))

    val expected = UniformVectorType(
      SpecificPairType(
        FlonumType,
        ExactIntegerType
      )
    )

    assert(InstantiateType(polyVec, reconciled) == expected)
  }
}
