package io.llambda.llvmir

import org.scalatest.FunSuite

class IrBlockSuite extends IrTestSuite {
  test("comment") {
    val block = new IrChildBlockBuilder(createTestFunction(), new LocalNameSource, "testLabel")
    
    block.comment("This is a test comment")

    assert(block.toIr === "testLabel:\n\t; This is a test comment")
  }

  test("trivial withMetadata") {
    val testFunction = createTestFunction()
    val block = new IrChildBlockBuilder(testFunction, new LocalNameSource, "testLabel")

    testFunction.withMetadata(Map("dbg" -> NumberedMetadata(1))) {
      block.retVoid
    }

    assertInstr(block, "ret void, !dbg !1")
  }
  
  test("multiple withMetadata") {
    val testFunction = createTestFunction()
    val block = new IrChildBlockBuilder(testFunction, new LocalNameSource, "testLabel")

    testFunction.withMetadata(Map(
      "a" -> NumberedMetadata(1),
      "c" -> NumberedMetadata(3),
      "b" -> NumberedMetadata(2)
    )) {
      block.retVoid
    }

    // This should be in alphabetical order
    assertInstr(block, "ret void, !a !1, !b !2, !c !3")
  }
  
  test("nested withMetadata") {
    val testFunction = createTestFunction()
    val block = new IrChildBlockBuilder(testFunction, new LocalNameSource, "testLabel")

    testFunction.withMetadata(Map(
      "a" -> NumberedMetadata(1),
      "b" -> NumberedMetadata(2)
    )) {
      block.alloca("unused")(DoubleType, 4)

      testFunction.withMetadata(Map("b" -> NumberedMetadata(15))) {
        block.alloca("unused")(FloatType, 6)
      }

      block.retVoid
    }

    // This should be in alphabetical order
    assertInstrs(block, List(
      "%unused1 = alloca double, i32 4, !a !1, !b !2",
      "%unused2 = alloca float, i32 6, !a !1, !b !15",
      "ret void, !a !1, !b !2"
    ))
  }

  test("adding instructions to terminated block fails") {
    val block = new IrChildBlockBuilder(createTestFunction(), new LocalNameSource, "testLabel")

    block.unreachable()

    intercept[InconsistentIrException] {
      block.retVoid()
    }
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
