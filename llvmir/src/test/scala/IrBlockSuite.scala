package io.llambda.llvmir

import org.scalatest.FunSuite

class IrBlockSuite extends IrTestSuite {
  test("comment") {
    val block = new IrChildBlockBuilder(new LocalNameSource, "testLabel")
    
    block.comment("This is a test comment")

    assert(block.toIr === "testLabel:\n\t; This is a test comment")
  }

  test("instr result in to predefined variable of same type") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val localVar = LocalVariable("predefinedVar", IntegerType(1))

    val block = createTestBlock()
    block.icmp(localVar)(ComparisonCond.Equal, None, var1, var2)

    assertInstr(block, "%predefinedVar = icmp eq i32 20, 30")
  }
  
  test("instr result in to predefined variable of different type fails") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val localVar = LocalVariable("predefinedVar", DoubleType)

    val block = createTestBlock()
    
    intercept[InconsistentIrException] {
      block.icmp(localVar)(ComparisonCond.Equal, None, var1, var2)
    }
  }
}
