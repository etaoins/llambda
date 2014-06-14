package io.llambda.llvmir.debug
import io.llambda

import org.scalatest.FunSuite

class SubroutineTypeMetadataSuite extends FunSuite {
  test("subroutine type") {
    assert(SubroutineTypeMetadata.toIrWithType === 
      """metadata !{i32 786453, i32 0, null, metadata !"", i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !{i32 0}, i32 0, null, null, null}"""
    )
  }
}
