package io.llambda.llvmir

import org.scalatest.FunSuite

class TbaaMetadataSuite extends FunSuite {
  private val testTbaaParent = NumberedMetadata(0)

  test("trivial root node") {
    val tbaaNode = TbaaMetadata("test node")

    assert(tbaaNode.toIr === """!{!"test node"}""")
  }

  test("constant root node") {
    val tbaaNode = TbaaMetadata("test node", constant=true)

    assert(tbaaNode.toIr === """!{!"test node", null, i64 1}""")
  }

  test("trivial child node") {
    val tbaaNode = TbaaMetadata("test node", Some(testTbaaParent))

    assert(tbaaNode.toIr === """!{!"test node", !0}""")
  }

  test("constant child node") {
    val tbaaNode = TbaaMetadata("test node", Some(testTbaaParent), constant=true)

    assert(tbaaNode.toIr === """!{!"test node", !0, i64 1}""")
  }
}
