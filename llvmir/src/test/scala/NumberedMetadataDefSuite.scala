package io.llambda.llvmir

import org.scalatest.FunSuite

class NumberedMetadataDefSuite extends FunSuite {
  test("trivial def") {
    val metadataDef = NumberedMetadataDef(
      index=12,
      metadataNode=UserDefinedMetadataNode(List(
        Some(IntegerConstant(IntegerType(32), 5)),
        None
      ))
    )

    assert(metadataDef.toIr === "!12 = !{i32 5, null}")
  }
}
