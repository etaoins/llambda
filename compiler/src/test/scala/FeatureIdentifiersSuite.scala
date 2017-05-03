package io.llambda.compiler

import org.scalatest.FunSuite


class FeatureIdentifiersSuite  extends FunSuite {
  test("basic features") {
    val expectedFeatures = Set(
      "llvm",
      "llambda",
      "llambda-0.3"
    )

    assert(FeatureIdentifiers() === expectedFeatures)
  }

  test("features with extras") {
    val expectedFeatures = Set(
      "llvm",
      "llambda",
      "llambda-0.3",
      "user-def-1",
      "user-def-2"
    )

    assert(FeatureIdentifiers(Set("user-def-1", "user-def-2")) === expectedFeatures)
  }
}
