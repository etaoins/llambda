package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

class TerminatorInstrsSuite extends IrTestSuite {
  test("ret with value") {
    val block = createTestBlock()
    block.ret(IntegerConstant(IntegerType(16), 45))

    assertInstr(block, "ret i16 45")
  }
  
  test("ret without value") {
    val block = createTestBlock()
    block.retVoid()

    assertInstr(block, "ret void")
  }

  test("valid conditional branch") {
    val block = createTestBlock()
    block.condBranch(IntegerConstant(IntegerType(1), 0), createTestBlock("true"), createTestBlock("false"))

    assertInstr(block, "br i1 0, label %true, label %false")
  }

  test("conditional branch with bad cond") {
    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.condBranch(StringConstant("Hello, world!"), createTestBlock("true"), createTestBlock("false"))
    }
  }

  test("unconditional branch") {
    val block = createTestBlock()
    block.uncondBranch(createTestBlock("alwayshere"))

    assertInstr(block, "br label %alwayshere")
  }

  test("unreachable") {
    val block = createTestBlock()
    block.unreachable()

    assertInstr(block, "unreachable")
  }
}
