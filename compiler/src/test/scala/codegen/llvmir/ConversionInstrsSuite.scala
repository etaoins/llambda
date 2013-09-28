package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class ConversionInstrsSuite extends IrTestSuite {
  test("trivial truncto") {
    val sourceValue = IntegerConstant(IntegerType(64), 50) 

    val block = createTestBlock()
    val resultVar = block.truncto("trivial")(sourceValue, IntegerType(32))
    
    assert(resultVar.irType === IntegerType(32))
    assertInstr(block, "%trivial1 = trunc i64 50 to i32")
  }
  
  test("truncto of same bit length") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.truncto("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("truncto to larger bit length") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.truncto("error")(sourceValue, IntegerType(64))
    }
  }
  
  test("truncto from non-int") {
    val sourceValue = DoubleConstant(145.0)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.truncto("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("trivial zextto") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()
    val resultVar = block.zextto("trivial")(sourceValue, IntegerType(64))
    
    assert(resultVar.irType === IntegerType(64))
    assertInstr(block, "%trivial1 = zext i32 50 to i64")
  }
  
  test("zextto of same bit length") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.zextto("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("zextto to smaller bit length") {
    val sourceValue = IntegerConstant(IntegerType(64), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.zextto("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("zextto from non-int") {
    val sourceValue = DoubleConstant(145.0)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.zextto("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("trivial sextto") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()
    val resultVar = block.sextto("trivial")(sourceValue, IntegerType(64))
    
    assert(resultVar.irType === IntegerType(64))
    assertInstr(block, "%trivial1 = sext i32 50 to i64")
  }
  
  test("sextto of same bit length") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.sextto("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("sextto to smaller bit length") {
    val sourceValue = IntegerConstant(IntegerType(64), 50) 

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.sextto("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("sextto from non-int") {
    val sourceValue = DoubleConstant(145.0)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.sextto("error")(sourceValue, IntegerType(32))
    }
  }

  test("trivial pointer bitcast") {
    val sourceValue = LocalVariable("fake", PointerType(IntegerType(8)))

    val block = createTestBlock()
    val resultVar = block.bitcastto("castpointer")(sourceValue, PointerType(IntegerType(64)))

    assert(resultVar.irType === PointerType(IntegerType(64)))
    assertInstr(block, "%castpointer1 = bitcast i8* %fake to i64*")
  }
  
  test("trivial value bitcast") {
    val sourceValue = IntegerConstant(IntegerType(32), 50)

    val block = createTestBlock()
    val resultVar = block.bitcastto("castvalue")(sourceValue, SingleType)

    assert(resultVar.irType === SingleType)
    assertInstr(block, "%castvalue1 = bitcast i32 50 to float")
  }
}
