package io.llambda.compiler
import io.llambda

import org.scalatest.FunSuite

class FeatureIdentifiersSuite  extends FunSuite {
  test("posix64be/llambda features") {
    val expectedFeatures = Set(
      "immutable-pairs",
      "exact-closed",
      "ieee-float",
      "full-unicode",
      "posix",
      "lp64",
      "big-endian"
    )

    assert(expectedFeatures subsetOf FeatureIdentifiers(platform.Posix64BE, dialect.Llambda))
  }

  test("posix43le/r7rs features") {
    val expectedFeatures = Set(
      "r7rs",
      "exact-closed",
      "ieee-float",
      "full-unicode",
      "posix",
      "ilp32",
      "little-endian"
    )

    assert(expectedFeatures subsetOf FeatureIdentifiers(platform.Posix32LE, dialect.R7RS))
  }

  test("win64/llambda features") {
    val expectedFeatures = Set(
      "immutable-pairs",
      "exact-closed",
      "ieee-float",
      "full-unicode",
      "windows",
      "llp64",
      "little-endian"
    )

    assert(expectedFeatures subsetOf FeatureIdentifiers(platform.Win64, dialect.Llambda))
  }

  test("posix64le/llambda features with extras") {
    val expectedFeatures = Set(
      "immutable-pairs",
      "exact-closed",
      "ieee-float",
      "full-unicode",
      "posix",
      "lp64",
      "big-endian",
      "user-def-1",
      "user-def-2"
    )

    assert(expectedFeatures subsetOf FeatureIdentifiers(
      platform.Posix64BE,
      dialect.Llambda,
      Set("user-def-1", "user-def-2")
    ))
  }
}
