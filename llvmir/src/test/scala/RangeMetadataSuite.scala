package io.llambda.llvmir

import org.scalatest.FunSuite

class RangeMetadataSuite extends FunSuite {
  test("trivial bool-like range") {
    // This would describe C++ boolean
    val booleanRange = RangeMetadata(IntegerType(8), (0, 2))

    assert(booleanRange.toIr === "!{i8 0, i8 2}")
  }

  test("wrapping range") {
    val wrappingRange = RangeMetadata(IntegerType(8), (255, 2))

    assert(wrappingRange.toIr === "!{i8 255, i8 2}")
  }

  test("multiple ranges") {
    val multipleRange = RangeMetadata(IntegerType(32), (0, 2), (4, 5))

    assert(multipleRange.toIr === "!{i32 0, i32 2, i32 4, i32 5}")
  }

  test("from no values fails") {
    intercept[InconsistentIrException] {
      RangeMetadata.fromPossibleValues(
        IntegerType(8),
        Nil
      )
    }
  }

  test("from single value") {
    val testRange = RangeMetadata.fromPossibleValues(
      IntegerType(8),
      List(1)
    )

    assert(testRange.map(_.toIr) === Some("!{i8 1, i8 2}"))
  }

  test("from disconnected value ranges") {
    val testRange = RangeMetadata.fromPossibleValues(
      IntegerType(8),
      List(0, 1, 2, 3, 100, 101, 103, 104, 105)
    )

    assert(testRange.map(_.toIr) === Some("!{i8 0, i8 4, i8 100, i8 102, i8 103, i8 106}"))
  }

  test("from disconnected value ranges with non-contiguous maximum integer value") {
    val testRange = RangeMetadata.fromPossibleValues(
      IntegerType(8),
      List(100, 101, 102, 103, 104, 255)
    )

    assert(testRange.map(_.toIr) === Some("!{i8 255, i8 0, i8 100, i8 105}"))
  }

  test("from disconnected value ranges with range crossing maximum integer value") {
    val testRange = RangeMetadata.fromPossibleValues(
      IntegerType(8),
      List(0, 1, 100, 101, 102, 103, 104, 255)
    )

    assert(testRange.map(_.toIr) === Some("!{i8 255, i8 2, i8 100, i8 105}"))
  }

  test("from all values for an integer type") {
    val testRange = RangeMetadata.fromPossibleValues(
      IntegerType(2),
      List(0, 1, 2, 3)
    )

    assert(!testRange.isDefined)
  }
}
