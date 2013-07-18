package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class MemoryInstrsSuite extends FunSuite {
  val i32 = IntegerType(32)

  test("trivial alloca") {
    val block = new IrBlock {
      val resultVar = alloca(IntegerType(32))

      assert(resultVar.irType === PointerType(IntegerType(32)))
    }

    assert(block.toIr === "\t%1 = alloca i32")
  }
  
  test("alloca with number of elements") {
    val block = new IrBlock {
      val resultVar = alloca(IntegerType(32), numElements=4)

      assert(resultVar.irType === PointerType(IntegerType(32)))
    }

    assert(block.toIr === "\t%1 = alloca i32, i32 4")
  }
  
  test("alloca with alignment") {
    val block = new IrBlock {
      val resultVar = alloca(IntegerType(32), alignment=1024)

      assert(resultVar.irType === PointerType(IntegerType(32)))
    }

    assert(block.toIr === "\t%1 = alloca i32, align 1024")
  }
  
  test("alloca with number of elements, alignment") {
    val block = new IrBlock {
      val resultVar = alloca(IntegerType(32), numElements=4, alignment=1024)

      assert(resultVar.irType === PointerType(IntegerType(32)))
    }

    assert(block.toIr === "\t%1 = alloca i32, i32 4, align 1024")
  }

  test("trivial load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(8)))
    
    val block = new IrBlock {
      val resultVar = load(fakePointer)
      assert(resultVar.irType === IntegerType(8))
    }

    assert(block.toIr === "\t%1 = load i8* %fake")
  }
  
  test("load from non-pointer") {
    val fakePointer = LocalVariable("fake", IntegerType(8))
    
    val block = new IrBlock {
      intercept[InternalCompilerErrorException] {
        load(fakePointer)
      }
    }
  }
  
  test("load from non-first class type") {
    val fakePointer = LocalVariable("fake", PointerType(FunctionType(i32, List(i32))))
    
    val block = new IrBlock {
      intercept[InternalCompilerErrorException] {
        load(fakePointer)
      }
    }
  }
  
  test("volatile load") {
    val fakePointer = LocalVariable("fake", PointerType(SingleType))
    
    val block = new IrBlock {
      val resultVar = load(
        from=fakePointer,
        volatile=true)

      assert(resultVar.irType === SingleType)
    }

    assert(block.toIr === "\t%1 = load volatile float* %fake")
  }
  
  test("aligned load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))
    
    val block = new IrBlock {
      val resultVar = load(
        from=fakePointer,
        alignment=1024)

      assert(resultVar.irType === IntegerType(32))
    }

    assert(block.toIr === "\t%1 = load i32* %fake, align 1024")
  }
  
  test("aligned volatile load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))
    
    val block = new IrBlock {
      val resultVar = load(
        from=fakePointer,
        volatile=true,
        alignment=1024)

      assert(resultVar.irType === IntegerType(32))
    }

    assert(block.toIr === "\t%1 = load volatile i32* %fake, align 1024")
  }
  
  test("trivial store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(8)))
    
    val block = new IrBlock {
      store(IntegerConstant(IntegerType(8), 1), fakePointer)
    }

    assert(block.toIr === "\tstore i8 1, i8* %fake")
  }
  
  test("store to non-pointer") {
    val fakePointer = LocalVariable("fake", IntegerType(8))
    
    val block = new IrBlock {
      intercept[InternalCompilerErrorException] {
        store(IntegerConstant(IntegerType(8), 1), fakePointer)
      }
    }
  }
  
  test("store to incompatible type") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(8)))
    
    val block = new IrBlock {
      intercept[InternalCompilerErrorException] {
        store(IntegerConstant(IntegerType(16), 1), fakePointer)
      }
    }
  }
  
  test("volatile store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(16)))
    
    val block = new IrBlock {
      store(IntegerConstant(IntegerType(16), 1), fakePointer, volatile=true)
    }

    assert(block.toIr === "\tstore volatile i16 1, i16* %fake")
  }
  
  test("aligned store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))
    
    val block = new IrBlock {
      store(IntegerConstant(IntegerType(32), 1), fakePointer, alignment=128)
    }

    assert(block.toIr === "\tstore i32 1, i32* %fake, align 128")
  }
  
  test("aligned volatile store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(64)))
    
    val block = new IrBlock {
      store(IntegerConstant(IntegerType(64), 1), fakePointer, volatile=true, alignment=128)
    }

    assert(block.toIr === "\tstore volatile i64 1, i64* %fake, align 128")
  }
}
