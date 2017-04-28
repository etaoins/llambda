package io.llambda.llvmir


class MemoryInstrsSuite extends IrTestSuite {
  val i32 = IntegerType(32)

  test("trivial alloca") {
    val block = createTestBlock()
    val resultVar = block.alloca("trivial")(IntegerType(32))

    assert(resultVar.irType === PointerType(IntegerType(32)))
    assertInstr(block, "%trivial1 = alloca i32")
  }

  test("alloca with number of elements") {
    val block = createTestBlock()
    val resultVar = block.alloca("numel")(IntegerType(32), numElements=IntegerConstant(IntegerType(16), 5))

    assert(resultVar.irType === PointerType(IntegerType(32)))
    assertInstr(block, "%numel1 = alloca i32, i16 5")
  }

  test("alloca with alignment") {
    val block = createTestBlock()
    val resultVar = block.alloca("align")(IntegerType(32), alignment=1024)

    assert(resultVar.irType === PointerType(IntegerType(32)))
    assertInstr(block, "%align1 = alloca i32, align 1024")
  }

  test("alloca with number of elements, alignment") {
    val block = createTestBlock()
    val resultVar = block.alloca("numelalign")(
      IntegerType(32), numElements=IntegerConstant(IntegerType(64), 4), alignment=1024
    )

    assert(resultVar.irType === PointerType(IntegerType(32)))
    assertInstr(block, "%numelalign1 = alloca i32, i64 4, align 1024")
  }

  test("trivial load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(8)))

    val block = createTestBlock()
    val resultVar = block.load("trivial")(fakePointer)

    assert(resultVar.irType === IntegerType(8))
    assertInstr(block, "%trivial1 = load i8, i8* %fake")
  }

  test("load from non-pointer") {
    val fakePointer = LocalVariable("fake", IntegerType(8))

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.load("error")(fakePointer)
    }
  }

  test("load from non-first class type") {
    val fakePointer = LocalVariable("fake", PointerType(FunctionType(i32, List(i32))))

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.load("error")(fakePointer)
    }
  }

  test("volatile load") {
    val fakePointer = LocalVariable("fake", PointerType(FloatType))

    val block = createTestBlock()
    val resultVar = block.load("vol")(
      from=fakePointer,
      volatile=true)

    assert(resultVar.irType === FloatType)
    assertInstr(block, "%vol1 = load volatile float, float* %fake")
  }

  test("aligned load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))

    val block = createTestBlock()
    val resultVar = block.load("align")(
      from=fakePointer,
      alignment=1024)

    assert(resultVar.irType === IntegerType(32))
    assertInstr(block, "%align1 = load i32, i32* %fake, align 1024")
  }

  test("tbaa load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))
    val tbaaNode = NumberedMetadata(5)

    val block = createTestBlock()
    val resultVar = block.load("align")(
      from=fakePointer,
      metadata=Map("tbaa" -> tbaaNode)
    )

    assert(resultVar.irType === IntegerType(32))
    assertInstr(block, "%align1 = load i32, i32* %fake, !tbaa !5")
  }

  test("aligned volatile tbaa load") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))
    val tbaaNode = NumberedMetadata(10)

    val block = createTestBlock()
    val resultVar = block.load("alignvol")(
      from=fakePointer,
      volatile=true,
      alignment=1024,
      metadata=Map("tbaa" -> tbaaNode)
    )

    assert(resultVar.irType === IntegerType(32))
    assertInstr(block, "%alignvol1 = load volatile i32, i32* %fake, align 1024, !tbaa !10")
  }

  test("trivial store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(8)))

    val block = createTestBlock()
    block.store(IntegerConstant(IntegerType(8), 1), fakePointer)

    assertInstr(block, "store i8 1, i8* %fake")
  }

  test("store to non-pointer") {
    val fakePointer = LocalVariable("fake", IntegerType(8))

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.store(IntegerConstant(IntegerType(8), 1), fakePointer)
    }
  }

  test("store to incompatible type") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(8)))

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.store(IntegerConstant(IntegerType(16), 1), fakePointer)
    }
  }

  test("volatile store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(16)))

    val block = createTestBlock()
    block.store(IntegerConstant(IntegerType(16), 1), fakePointer, volatile=true)

    assertInstr(block, "store volatile i16 1, i16* %fake")
  }

  test("aligned store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))

    val block = createTestBlock()
    block.store(IntegerConstant(IntegerType(32), 1), fakePointer, alignment=128)

    assertInstr(block, "store i32 1, i32* %fake, align 128")
  }

  test("tbaa store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))
    val tbaaNode = NumberedMetadata(0)

    val block = createTestBlock()
    block.store(
      IntegerConstant(IntegerType(32), 1),
      fakePointer,
      metadata=Map("tbaa" -> tbaaNode)
    )

    assertInstr(block, "store i32 1, i32* %fake, !tbaa !0")
  }

  test("aligned volatile tbaa store") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(64)))
    val tbaaNode = NumberedMetadata(45)

    val block = createTestBlock()
    block.store(
      IntegerConstant(IntegerType(64), 1),
      fakePointer,
      volatile=true,
      alignment=128,
      metadata=Map("tbaa" -> tbaaNode)
    )

    assertInstr(block, "store volatile i64 1, i64* %fake, align 128, !tbaa !45")
  }

  test("0 index getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = createTestBlock()
    val resultVar = block.getelementptr("zeroindex")(
      elementType=IntegerType(8),
      basePointer=fakePointer,
      indices=List()
    )

    assert(resultVar.irType === PointerType(IntegerType(8)))
    assertInstr(block, "%zeroindex1 = getelementptr %opaqueType, %opaqueType* %fake")
  }

  test("getelementptr from non-pointer") {
    val fakeNonPointer = LocalVariable("fake", IntegerType(8))

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.getelementptr("error")(
        elementType=IntegerType(8),
        basePointer=fakeNonPointer,
        indices=List()
      )
    }
  }

  test("non-integer index getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.getelementptr("error")(
        elementType=IntegerType(16),
        basePointer=fakePointer,
        indices=List(IntegerConstant(IntegerType(32), 4), StringConstant.fromUtf8String("HELLO"))
      )
    }
  }

  test("1 index getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = createTestBlock()
    val resultVar = block.getelementptr("oneindex")(
      elementType=UserDefinedType("opaqueType"),
      basePointer=fakePointer,
      indices=IntegerConstant(IntegerType(32), 42) :: Nil
    )

    assert(resultVar.irType === PointerType(UserDefinedType("opaqueType")))
    assertInstr(block, "%oneindex1 = getelementptr %opaqueType, %opaqueType* %fake, i32 42")
  }

  test("1 index getelementptr with mismatched type") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.getelementptr("oneindexMismatch")(
        elementType=IntegerType(16),
        basePointer=fakePointer,
        indices=IntegerConstant(IntegerType(32), 42) :: Nil
      )
    }
  }

  test("inbounds getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(IntegerType(32)))

    val block = createTestBlock()
    val resultVar = block.getelementptr("inbounds")(
      elementType=IntegerType(32),
      basePointer=fakePointer,
      indices=IntegerConstant(IntegerType(64), 23) :: Nil,
      inbounds=true
    )

    assert(resultVar.irType === PointerType(IntegerType(32)))
    assertInstr(block, "%inbounds1 = getelementptr inbounds i32, i32* %fake, i64 23")
  }

  test("2 index getelementptr") {
    val fakePointer = LocalVariable("fake", PointerType(UserDefinedType("opaqueType")))

    val block = createTestBlock()
    val resultVar = block.getelementptr("twoindex")(
      elementType=IntegerType(64),
      basePointer=fakePointer,
      indices=List(42, 23).map(IntegerConstant(IntegerType(32), _))
    )

    assert(resultVar.irType === PointerType(IntegerType(64)))
    assertInstr(block, "%twoindex1 = getelementptr %opaqueType, %opaqueType* %fake, i32 42, i32 23")
  }
}
