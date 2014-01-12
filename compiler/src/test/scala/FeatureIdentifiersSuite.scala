package io.llambda.compiler
import io.llambda

import org.scalatest.FunSuite

class FeatureIdentifiersSuite  extends FunSuite {
  test("posix64be features") {
    val expectedFeatures = Set(
      "r7rs",
      "ieee-float",
      "full-unicode",
      "posix",
      "lp64",
      "big-endian"
    )

    assert(expectedFeatures subsetOf FeatureIdentifiers(platform.Posix64BE))
  }
  
  test("posix43le features") {
    val expectedFeatures = Set(
      "r7rs",
      "ieee-float",
      "full-unicode",
      "posix",
      "ilp32",
      "little-endian"
    )

    assert(expectedFeatures subsetOf FeatureIdentifiers(platform.Posix32LE))
  }
  
  test("win64 features") {
    val expectedFeatures = Set(
      "r7rs",
      "ieee-float",
      "full-unicode",
      "windows",
      "llp64",
      "little-endian"
    )

    assert(expectedFeatures subsetOf FeatureIdentifiers(platform.Win64))
  }
  
  test("posix64le features with extras") {
    val expectedFeatures = Set(
      "r7rs",
      "ieee-float",
      "full-unicode",
      "posix",
      "lp64",
      "big-endian",
      "user-def-1",
      "user-def-2"
    )

    assert(expectedFeatures subsetOf FeatureIdentifiers(platform.Posix64BE, Set("user-def-1", "user-def-2")))
  }
}
