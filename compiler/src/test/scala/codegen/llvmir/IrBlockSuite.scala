package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class IrBlockSuite extends FunSuite {
  test("comment") {
    val block = new IrBlockBuilder {
      comment("This is a test comment")
    }

    assert(block.toIr === "\t; This is a test comment")
  }
}
