package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class ConversionInstrsSuite extends IrTestSuite {
  test("trivial truncTo") {
    val sourceValue = IntegerConstant(IntegerType(64), 50) 

    val block = createTestBlock()
    val resultVar = block.truncTo("trivial")(sourceValue, IntegerType(32))
    
    assert(resultVar.irType === IntegerType(32))
    assertInstr(block, "%trivial1 = trunc i64 50 to i32")
  }
  
  test("truncTo of same bit length") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.truncTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("truncTo to larger bit length") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.truncTo("error")(sourceValue, IntegerType(64))
    }
  }
  
  test("truncTo from non-int") {
    val sourceValue = DoubleConstant(145.0)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.truncTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("trivial zextTo") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()
    val resultVar = block.zextTo("trivial")(sourceValue, IntegerType(64))
    
    assert(resultVar.irType === IntegerType(64))
    assertInstr(block, "%trivial1 = zext i32 50 to i64")
  }
  
  test("zextTo of same bit length") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.zextTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("zextTo to smaller bit length") {
    val sourceValue = IntegerConstant(IntegerType(64), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.zextTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("zextTo from non-int") {
    val sourceValue = DoubleConstant(145.0)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.zextTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("trivial sextTo") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()
    val resultVar = block.sextTo("trivial")(sourceValue, IntegerType(64))
    
    assert(resultVar.irType === IntegerType(64))
    assertInstr(block, "%trivial1 = sext i32 50 to i64")
  }
  
  test("sextTo of same bit length") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.sextTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("sextTo to smaller bit length") {
    val sourceValue = IntegerConstant(IntegerType(64), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.sextTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("sextTo from non-int") {
    val sourceValue = DoubleConstant(145.0)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.sextTo("error")(sourceValue, IntegerType(32))
    }
  }

  test("trivial pointer bitcast") {
    val sourceValue = LocalVariable("fake", PointerType(IntegerType(8)))

    val block = createTestBlock()
    val resultVar = block.bitcastTo("castpointer")(sourceValue, PointerType(IntegerType(64)))

    assert(resultVar.irType === PointerType(IntegerType(64)))
    assertInstr(block, "%castpointer1 = bitcast i8* %fake to i64*")
  }
  
  test("trivial value bitcast") {
    val sourceValue = IntegerConstant(IntegerType(32), 50)

    val block = createTestBlock()
    val resultVar = block.bitcastTo("castvalue")(sourceValue, FloatType)

    assert(resultVar.irType === FloatType)
    assertInstr(block, "%castvalue1 = bitcast i32 50 to float")
  }
}
