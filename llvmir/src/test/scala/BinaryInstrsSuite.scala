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
  
  test("trivial fadd") {
    val block = createTestBlock()

    val op1 = FloatConstant(14.5f)
    val op2 = FloatConstant(-100.0f)

    val resultVar = block.fadd("add")(Set(), op1, op2)

    assert(resultVar.irType === FloatType)
    assertInstr(block, "%add1 = fadd float 14.5, -100.0")
  }
  
  test("fadd with mismatched type fails") {
    val block = createTestBlock()

    val op1 = FloatConstant(14.5f)
    val op2 = DoubleConstant(-100.0)

    intercept[InconsistentIrException] {
      block.fadd("mismatched")(Set(), op1, op2)
    }
  }
  
  test("fadd with non-floats fails") {
    val block = createTestBlock()

    val op1 = IntegerConstant(IntegerType(32), 5)
    val op2 = IntegerConstant(IntegerType(32), -10)

    intercept[InconsistentIrException] {
      block.fadd("nonfloat")(Set(), op1, op2)
    }
  }

  test("fsub with nnan") {
    val block = createTestBlock()

    val op1 = DoubleConstant(14.5)
    val op2 = DoubleConstant(-100.0)

    val resultVar = block.fsub("sub")(Set(FastMathFlag.NoNaN), op1, op2)

    assert(resultVar.irType === DoubleType)
    assertInstr(block, "%sub1 = fsub nnan double 14.5, -100.0")
  }
  
  test("fmul with ninf and arcp") {
    val block = createTestBlock()

    val op1 = DoubleConstant(14.5)
    val op2 = DoubleConstant(-100.0)

    val resultVar = block.fsub("mul")(Set(FastMathFlag.NoInf, FastMathFlag.AllowReciprocal), op1, op2)

    assert(resultVar.irType === DoubleType)
    assertInstr(block, "%mul1 = fsub arcp ninf double 14.5, -100.0")
  }
  
  test("fdiv with nsz and fast") {
    val block = createTestBlock()

    val op1 = DoubleConstant(14.5)
    val op2 = DoubleConstant(-100.0)

    val resultVar = block.fdiv("div")(Set(FastMathFlag.NoSignedZero, FastMathFlag.Fast), op1, op2)

    assert(resultVar.irType === DoubleType)
    assertInstr(block, "%div1 = fdiv fast nsz double 14.5, -100.0")
  }
}
