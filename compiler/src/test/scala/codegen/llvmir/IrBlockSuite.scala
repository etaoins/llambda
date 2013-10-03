package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class IrBlockSuite extends FunSuite {
  test("comment") {
    val block = new IrChildBlockBuilder(new LocalNameSource, "testLabel")
    
    block.comment("This is a test comment")

    assert(block.toIr === "testLabel:\n\t; This is a test comment")
  }
}
