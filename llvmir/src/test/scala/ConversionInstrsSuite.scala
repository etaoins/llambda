package io.llambda.llvmir

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

    intercept[InconsistentIrException] {
      block.truncTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("truncTo to larger bit length") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.truncTo("error")(sourceValue, IntegerType(64))
    }
  }
  
  test("truncTo from non-int") {
    val sourceValue = DoubleConstant(145.0)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
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

    intercept[InconsistentIrException] {
      block.zextTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("zextTo to smaller bit length") {
    val sourceValue = IntegerConstant(IntegerType(64), 50) 

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.zextTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("zextTo from non-int") {
    val sourceValue = DoubleConstant(145.0)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
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

    intercept[InconsistentIrException] {
      block.sextTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("sextTo to smaller bit length") {
    val sourceValue = IntegerConstant(IntegerType(64), 50) 

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.sextTo("error")(sourceValue, IntegerType(32))
    }
  }
  
  test("sextTo from non-int") {
    val sourceValue = DoubleConstant(145.0)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
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

  test("trivial fptruncTo") {
    val sourceValue = DoubleConstant(60.0)

    val block = createTestBlock()
    val resultVar = block.fptruncTo("trivial")(sourceValue, FloatType)
    
    assert(resultVar.irType === FloatType)
    assertInstr(block, "%trivial1 = fptrunc double 60.0 to float")
  }
  
  test("fptruncTo of same bit length") {
    val sourceValue = FloatConstant(50.0f)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.fptruncTo("error")(sourceValue, FloatType)
    }
  }
  
  test("fptruncTo to larger bit length") {
    val sourceValue = FloatConstant(50.0f)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.fptruncTo("error")(sourceValue, DoubleType)
    }
  }
  
  test("truncTo from non-float") {
    val sourceValue = IntegerConstant(IntegerType(64), 145)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.fptruncTo("error")(sourceValue, FloatType)
    }
  }
  
  test("trivial fpextTo") {
    val sourceValue = FloatConstant(50.0f)

    val block = createTestBlock()
    val resultVar = block.fpextTo("trivial")(sourceValue, DoubleType)
    
    assert(resultVar.irType === DoubleType)
    assertInstr(block, "%trivial1 = fpext float 50.0 to double")
  }
  
  test("fpextTo of same bit length") {
    val sourceValue = FloatConstant(50.0f)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.fpextTo("error")(sourceValue, FloatType)
    }
  }
  
  test("fpextTo to smaller bit length") {
    val sourceValue = DoubleConstant(50.0f)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.fpextTo("error")(sourceValue, FloatType)
    }
  }
  
  test("fpextTo from non-float") {
    val sourceValue = IntegerConstant(IntegerType(32), 145)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.fpextTo("error")(sourceValue, DoubleType)
    }
  }
  
  test("trivial uitofp") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()
    val resultVar = block.uitofp("trivial")(sourceValue, FloatType)
    
    assert(resultVar.irType === FloatType)
    assertInstr(block, "%trivial1 = uitofp i32 50 to float")
  }
  
  test("uitofp from non-integer") {
    val sourceValue = DoubleConstant(50.0)

    val block = createTestBlock()
    
    intercept[InconsistentIrException] {
      val resultVar = block.uitofp("trivial")(sourceValue, FloatType)
    }
  }
 
  test("trivial sitofp") {
    val sourceValue = IntegerConstant(IntegerType(32), 50) 

    val block = createTestBlock()
    val resultVar = block.sitofp("trivial")(sourceValue, DoubleType)
    
    assert(resultVar.irType === DoubleType)
    assertInstr(block, "%trivial1 = sitofp i32 50 to double")
  }
  
  test("sitofp from non-integer") {
    val sourceValue = DoubleConstant(50.0)

    val block = createTestBlock()
    
    intercept[InconsistentIrException] {
      val resultVar = block.sitofp("trivial")(sourceValue, FloatType)
    }
  }

  test("trivial ptrtoint") {
    val sourceValue = NullPointerConstant(PointerType(DoubleType))
    val block = createTestBlock()

    val resultVar = block.ptrtoint("trivial")(sourceValue, IntegerType(32))

    assert(resultVar.irType === IntegerType(32))
    assertInstr(block, "%trivial1 = ptrtoint double* null to i32")
  }

  test("ptrtoint with non-pointer fails") {
    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.ptrtoint("fails")(IntegerConstant(IntegerType(64), 5), IntegerType(64))
    }
  }
  
  test("trivial inttoptr") {
    val sourceValue = IntegerConstant(IntegerType(64), 67)
    val block = createTestBlock()

    val resultVar = block.inttoptr("trivial")(sourceValue, PointerType(FloatType))

    assert(resultVar.irType === PointerType(FloatType))
    assertInstr(block, "%trivial1 = inttoptr i64 67 to float*")
  }
  
  test("inttoptr with non-integer fails") {
    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.inttoptr("fails")(DoubleConstant(50.0), PointerType(FloatType))
    }
  }
}
