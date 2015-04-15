package io.llambda.llvmir.debug
import io.llambda

import org.scalatest.FunSuite

import llambda.llvmir._

class LocationMetadataSuite extends FunSuite {
  test("trivial non-inlining location") {
    val fakeScope = NumberedMetadata(1)

    val locationNode = LocationMetadata(
      line=12,
      column=4,
      scope=fakeScope,
      originalScopeOpt=None
    )

    assert(locationNode.toIr === "!{i32 12, i32 4, !1, null}")
  }

  test("trivial inlined location") {
    val fakeScope = NumberedMetadata(1)
    val fakeOriginalScope = NumberedMetadata(2)

    val locationNode = LocationMetadata(
      line=12,
      column=4,
      scope=fakeScope,
      originalScopeOpt=Some(fakeOriginalScope)
    )

    assert(locationNode.toIr === "!{i32 12, i32 4, !1, !2}")
  }
}
