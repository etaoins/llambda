package io.llambda.llvmir

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

    intercept[InconsistentIrException] {
      block.condBranch(StringConstant.fromUtf8String("Hello, world!"), createTestBlock("true"), createTestBlock("false"))
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

  test("empty switch") {
    val block = createTestBlock()

    val testValue = IntegerConstant(IntegerType(32), 0) 
    val defaultBlock = createTestBlock("default")

    block.switch(testValue, defaultBlock)

    assertInstr(block, "switch i32 0, label %default [  ]")
  }
  
  test("switch with non-integer types fails") {
    val block = createTestBlock()

    val testValue = DoubleConstant(15.0)
    val defaultBlock = createTestBlock("default")

    intercept[InconsistentIrException] {
      block.switch(testValue, defaultBlock)
    }
  }
  
  test("switch with one target") {
    val block = createTestBlock()

    val testValue = IntegerConstant(IntegerType(32), 0) 
    val defaultBlock = createTestBlock("default")
    val blockOne = createTestBlock("blockOne")

    block.switch(testValue, defaultBlock,
      (5L -> blockOne)
    )

    assertInstr(block, "switch i32 0, label %default [ i32 5, label %blockOne ]")
  }
  
  test("switch with two targets") {
    val block = createTestBlock()

    val testValue = IntegerConstant(IntegerType(16), 0) 
    val defaultBlock = createTestBlock("default")
    val blockOne = createTestBlock("blockOne")
    val blockTwo = createTestBlock("blockTwo")

    block.switch(testValue, defaultBlock,
      (1L -> blockOne),
      (2L -> blockTwo)
    )

    assertInstr(block, "switch i16 0, label %default [ i16 1, label %blockOne  i16 2, label %blockTwo ]")
  }
  
  test("switch with duplicate comparison constant fails") {
    val block = createTestBlock()

    val testValue = IntegerConstant(IntegerType(16), 0) 
    val defaultBlock = createTestBlock("default")
    val blockOne = createTestBlock("blockOne")
    val blockTwo = createTestBlock("blockTwo")

    intercept[InconsistentIrException] {
      block.switch(testValue, defaultBlock,
        (1L -> blockOne),
        (1L -> blockTwo)
      )
    }
  }
}
