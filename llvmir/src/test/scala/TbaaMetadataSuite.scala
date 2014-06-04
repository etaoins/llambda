package io.llambda.llvmir

import org.scalatest.FunSuite

class TbaaMetadataSuite extends FunSuite {
  private val testTbaaParent = MetadataDef(
    0,
    TbaaMetadata("root node")
  ).namedMetadata

  test("trivial root node") {
    val tbaaNode = TbaaMetadata("test node")

    assert(tbaaNode.toIrWithType === """metadata !{metadata !"test node"}""")
  }
  
  test("constant root node") {
    val tbaaNode = TbaaMetadata("test node", constant=true)

    assert(tbaaNode.toIrWithType === """metadata !{metadata !"test node", null, i64 1}""")
  }
  
  test("trivial child node") {
    val parentNode = TbaaMetadata

    val tbaaNode = TbaaMetadata("test node", Some(testTbaaParent))

    assert(tbaaNode.toIrWithType === """metadata !{metadata !"test node", metadata !0}""")
  }
  
  test("constant child node") {
    val tbaaNode = TbaaMetadata("test node", Some(testTbaaParent), constant=true)

    assert(tbaaNode.toIrWithType === """metadata !{metadata !"test node", metadata !0, i64 1}""")
  }
}
