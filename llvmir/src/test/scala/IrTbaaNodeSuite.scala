package io.llambda.llvmir

import org.scalatest.FunSuite

class IrTbaaNodeSuite extends FunSuite {
  test("trivial root node") {
    val tbaaNode = IrTbaaNode(0, "test node")

    assert(tbaaNode.toIr === """!0 = metadata !{ metadata !"test node" }""")
  }
  
  test("constant root node") {
    val tbaaNode = IrTbaaNode(0, "test node", constant=true)

    assert(tbaaNode.toIr === """!0 = metadata !{ metadata !"test node", null, i64 1 }""")
  }
  
  test("trivial child node") {
    val tbaaNode = IrTbaaNode(1, "test node", Some(0))

    assert(tbaaNode.toIr === """!1 = metadata !{ metadata !"test node", metadata !0 }""")
  }
  
  test("constant child node") {
    val tbaaNode = IrTbaaNode(1, "test node", Some(0), constant=true)

    assert(tbaaNode.toIr === """!1 = metadata !{ metadata !"test node", metadata !0, i64 1 }""")
  }
}
