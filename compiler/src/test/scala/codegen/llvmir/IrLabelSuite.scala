package llambda.codegen.llvmir

import org.scalatest.FunSuite

class IrLabelSuite extends FunSuite {
  test("basic label") {
    assert(IrLabel("test").toIr === "%test")
  }
}

