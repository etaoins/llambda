package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class MemoryInstrsSuite extends FunSuite {
  val i32 = IntegerType(32)

  test("trivial alloca") {
    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = alloca("trivial")(IntegerType(32))

      assert(resultVar.irType === PointerType(IntegerType(32)))
    }

    assert(block.toIr === "\t%trivial1 = alloca i32")
  }
  
  test("alloca with number of elements") {
    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = alloca("numel")(IntegerType(32), numElements=4)

      assert(resultVar.irType === PointerType(IntegerType(32)))
    }

    assert(block.toIr === "\t%numel1 = alloca i32, i32 4")
  }
  
  test("alloca with alignment") {
    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = alloca("align")(IntegerType(32), alignment=1024)

      assert(resultVar.irType === PointerType(IntegerType(32)))
    }

    assert(block.toIr === "\t%align1 = alloca i32, align 1024")
  }
  
  test("alloca with number of elements, alignment") {
    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = alloca("numelalign")(
        IntegerType(32), numElements=4, alignment=1024
      )

      assert(resultVar.irType === PointerType(IntegerType(32)))
    }

    assert(block.toIr === "\t%numelalign1 = alloca i32, i32 4, align 1024")
  }

  test("trivial load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(8)))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = load("trivial")(fakePointer)
      assert(resultVar.irType === IntegerType(8))
    }

    assert(block.toIr === "\t%trivial1 = load i8* %fake")
  }
  
  test("load from non-pointer") {
    val fakePointer = LocalVariable("fake", IntegerType(8))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        load("error")(fakePointer)
      }
    }
  }
  
  test("load from non-first class type") {
    val fakePointer = LocalVariable("fake", PointerType(FunctionType(i32, List(i32))))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        load("error")(fakePointer)
      }
    }
  }
  
  test("volatile load") {
    val fakePointer = LocalVariable("fake", PointerType(SingleType))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = load("vol")(
        from=fakePointer,
        volatile=true)

      assert(resultVar.irType === SingleType)
    }

    assert(block.toIr === "\t%vol1 = load volatile float* %fake")
  }
  
  test("aligned load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = load("align")(
        from=fakePointer,
        alignment=1024)

      assert(resultVar.irType === IntegerType(32))
    }

    assert(block.toIr === "\t%align1 = load i32* %fake, align 1024")
  }
  
  test("aligned volatile load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = load("alignvol")(
        from=fakePointer,
        volatile=true,
        alignment=1024)

      assert(resultVar.irType === IntegerType(32))
    }

    assert(block.toIr === "\t%alignvol1 = load volatile i32* %fake, align 1024")
  }
  
  test("trivial store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(8)))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      store(IntegerConstant(IntegerType(8), 1), fakePointer)
    }

    assert(block.toIr === "\tstore i8 1, i8* %fake")
  }
  
  test("store to non-pointer") {
    val fakePointer = LocalVariable("fake", IntegerType(8))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        store(IntegerConstant(IntegerType(8), 1), fakePointer)
      }
    }
  }
  
  test("store to incompatible type") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(8)))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        store(IntegerConstant(IntegerType(16), 1), fakePointer)
      }
    }
  }
  
  test("volatile store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(16)))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      store(IntegerConstant(IntegerType(16), 1), fakePointer, volatile=true)
    }

    assert(block.toIr === "\tstore volatile i16 1, i16* %fake")
  }
  
  test("aligned store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      store(IntegerConstant(IntegerType(32), 1), fakePointer, alignment=128)
    }

    assert(block.toIr === "\tstore i32 1, i32* %fake, align 128")
  }
  
  test("aligned volatile store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(64)))
    
    val block = new IrBlockBuilder()(new LocalNameSource) {
      store(IntegerConstant(IntegerType(64), 1), fakePointer, volatile=true, alignment=128)
    }

    assert(block.toIr === "\tstore volatile i64 1, i64* %fake, align 128")
  }

  test("0 index getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = getelementptr("zeroindex")(
        resultType=IntegerType(8),
        basePointer=fakePointer,
        indices=List()
      )

      assert(resultVar.irType === IntegerType(8))
    }

    assert(block.toIr === "\t%zeroindex1 = getelementptr %opaqueType* %fake") 
  }
  
  test("getelementptr from non-pointer") {
    val fakeNonPointer = LocalVariable("fake", IntegerType(8))

    val block = new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        val resultVar = getelementptr("error")(
          resultType=IntegerType(8),
          basePointer=fakeNonPointer,
          indices=List()
        )
      }
    }
  }
  
  test("non-integer index getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        getelementptr("error")(
          resultType=IntegerType(16),
          basePointer=fakePointer,
          indices=List(StringConstant("HELLO"))
        )
      }
    }
  }
  
  test("1 index getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = getelementptr("oneindex")(
        resultType=IntegerType(16),
        basePointer=fakePointer,
        indices=IntegerConstant(IntegerType(32), 42) :: Nil
      )

      assert(resultVar.irType === IntegerType(16))
    }

    assert(block.toIr === "\t%oneindex1 = getelementptr %opaqueType* %fake, i32 42") 
  }
  
  test("inbounds getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = getelementptr("inbounds")(
        resultType=IntegerType(32),
        basePointer=fakePointer,
        indices=IntegerConstant(IntegerType(64), 23) :: Nil,
        inbounds=true
      )

      assert(resultVar.irType === IntegerType(32))
    }

    assert(block.toIr === "\t%inbounds1 = getelementptr inbounds %opaqueType* %fake, i64 23") 
  }
  
  test("2 index getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = getelementptr("twoindex")(
        resultType=IntegerType(64),
        basePointer=fakePointer,
        indices=List(42, 23).map(IntegerConstant(IntegerType(32), _))
      )

      assert(resultVar.irType === IntegerType(64))
    }

    assert(block.toIr === "\t%twoindex1 = getelementptr %opaqueType* %fake, i32 42, i32 23") 
  }
}
