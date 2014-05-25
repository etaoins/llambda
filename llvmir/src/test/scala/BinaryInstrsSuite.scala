package io.llambda.llvmir

import org.scalatest.FunSuite

class BinaryInstrsSuite extends IrTestSuite {
  test("trivial add") {
    val block = createTestBlock()

    val op1 = IntegerConstant(IntegerType(32), 12) 
    val op2 = IntegerConstant(IntegerType(32), 1) 

    val resultVar = block.add("trivial")(Set(), op1, op2)

    assert(resultVar.irType === IntegerType(32))
    assertInstr(block, "%trivial1 = add i32 12, 1")
  }
  
  test("add with nsw") {
    val block = createTestBlock()

    val op1 = IntegerConstant(IntegerType(16), 12) 
    val op2 = IntegerConstant(IntegerType(16), 1) 

    val resultVar = block.add("nswtest")(Set(WrapBehaviour.NoSignedWrap), op1, op2)

    assert(resultVar.irType === IntegerType(16))
    assertInstr(block, "%nswtest1 = add nsw i16 12, 1")
  }
  
  test("add with nuw") {
    val block = createTestBlock()

    val op1 = IntegerConstant(IntegerType(64), 12) 
    val op2 = IntegerConstant(IntegerType(64), 1) 

    val resultVar = block.add("nuwtest")(Set(WrapBehaviour.NoUnsignedWrap), op1, op2)

    assert(resultVar.irType === IntegerType(64))
    assertInstr(block, "%nuwtest1 = add nuw i64 12, 1")
  }
  
  test("add with nsw and nuw") {
    val block = createTestBlock()

    val op1 = IntegerConstant(IntegerType(8), 12) 
    val op2 = IntegerConstant(IntegerType(8), 1) 

    val resultVar = block.add("nswnuwtest")(Set(WrapBehaviour.NoUnsignedWrap, WrapBehaviour.NoSignedWrap), op1, op2)

    assert(resultVar.irType === IntegerType(8))
    assertInstr(block, "%nswnuwtest1 = add nsw nuw i8 12, 1")
  }
  
  test("add of non-ints fails") {
    val op1 = DoubleConstant(12) 
    val op2 = DoubleConstant(1) 

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.add("error")(Set(), op1, op2)
    }
  }
  
  test("add of mistmached types fails") {
    val op1 = IntegerConstant(IntegerType(32), 12) 
    val op2 = IntegerConstant(IntegerType(64), 1)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.add("error")(Set(), op1, op2)
    }
  }
  
  test("sub with nsw") {
    val block = createTestBlock()

    val op1 = IntegerConstant(IntegerType(16), 12) 
    val op2 = IntegerConstant(IntegerType(16), 1) 

    val resultVar = block.sub("nswtest")(Set(WrapBehaviour.NoSignedWrap), op1, op2)

    assert(resultVar.irType === IntegerType(16))
    assertInstr(block, "%nswtest1 = sub nsw i16 12, 1")
  }
  
  test("mul with nuw") {
    val block = createTestBlock()

    val op1 = IntegerConstant(IntegerType(64), 12) 
    val op2 = IntegerConstant(IntegerType(64), 1) 

    val resultVar = block.mul("nuwtest")(Set(WrapBehaviour.NoUnsignedWrap), op1, op2)

    assert(resultVar.irType === IntegerType(64))
    assertInstr(block, "%nuwtest1 = mul nuw i64 12, 1")
  }
}
