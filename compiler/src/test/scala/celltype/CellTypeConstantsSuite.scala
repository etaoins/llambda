package io.llambda.compiler.celltype
import io.llambda

import org.scalatest.FunSuite
import llambda.compiler.platform.ExamplePlatform


class CellTypeConstantSuite extends FunSuite {
  test("VectorCellConstants.maxPossibleLength") {
    assertResult(2305843009213693952L) {
      VectorCellConstants.maxPossibleLength(ExamplePlatform.`x86_64-apple-macosx10.10.0`)
    }

    assertResult(1073741824L) {
      VectorCellConstants.maxPossibleLength(ExamplePlatform.`i386-unknown-freebsd10.1`)
    }
  }

  test("BytevectorCellConstants.maxPossibleLength") {
    assertResult(Long.MaxValue) {
      BytevectorCellConstants.maxPossibleLength(ExamplePlatform.`x86_64-apple-macosx10.10.0`)
    }

    assertResult(4294967296L) {
      BytevectorCellConstants.maxPossibleLength(ExamplePlatform.`i386-unknown-freebsd10.1`)
    }
  }
}
