package io.llambda.llvmir

import org.scalatest.FunSuite

class MetadataDefSuite extends FunSuite {
  test("trivial def") {
    val metadataDef = MetadataDef(
      index=12,
      metadataNode=UserDefinedMetadataNode(List(
        Some(IntegerConstant(IntegerType(32), 5)),
        None
      ))
    )

    assert(metadataDef.toIr === "!12 = metadata !{i32 5, null}")
  }
}
