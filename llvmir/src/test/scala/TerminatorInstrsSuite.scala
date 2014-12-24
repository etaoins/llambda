package io.llambda.llvmir

class TerminatorInstrsSuite extends IrTestSuite {
  test("ret with value") {
    val block = createTestBlock()
    block.ret(IntegerConstant(IntegerType(16), 45))

    assertInstr(block, "ret i16 45")
  }
  
  test("ret without value") {
    val block = createTestBlock()
    block.retVoid()

    assertInstr(block, "ret void")
  }

  test("valid conditional branch") {
    val block = createTestBlock()
    block.condBranch(IntegerConstant(IntegerType(1), 0), createTestBlock("true"), createTestBlock("false"))

    assertInstr(block, "br i1 0, label %true, label %false")
  }

  test("conditional branch with bad cond") {
    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.condBranch(StringConstant.fromUtf8String("Hello, world!"), createTestBlock("true"), createTestBlock("false"))
    }
  }

  test("unconditional branch") {
    val block = createTestBlock()
    block.uncondBranch(createTestBlock("alwayshere"))

    assertInstr(block, "br label %alwayshere")
  }

  test("unreachable") {
    val block = createTestBlock()
    block.unreachable()

    assertInstr(block, "unreachable")
  }

  test("empty switch") {
    val block = createTestBlock()

    val testValue = IntegerConstant(IntegerType(32), 0) 
    val defaultBlock = createTestBlock("default")

    block.switch(testValue, defaultBlock)

    assertInstr(block, "switch i32 0, label %default [  ]")
  }
  
  test("switch with non-integer types fails") {
    val block = createTestBlock()

    val testValue = DoubleConstant(15.0)
    val defaultBlock = createTestBlock("default")

    intercept[InconsistentIrException] {
      block.switch(testValue, defaultBlock)
    }
  }
  
  test("switch with one target") {
    val block = createTestBlock()

    val testValue = IntegerConstant(IntegerType(32), 0) 
    val defaultBlock = createTestBlock("default")
    val blockOne = createTestBlock("blockOne")

    block.switch(testValue, defaultBlock,
      (5L -> blockOne)
    )

    assertInstr(block, "switch i32 0, label %default [ i32 5, label %blockOne ]")
  }
  
  test("switch with two targets") {
    val block = createTestBlock()

    val testValue = IntegerConstant(IntegerType(16), 0) 
    val defaultBlock = createTestBlock("default")
    val blockOne = createTestBlock("blockOne")
    val blockTwo = createTestBlock("blockTwo")

    block.switch(testValue, defaultBlock,
      (1L -> blockOne),
      (2L -> blockTwo)
    )

    assertInstr(block, "switch i16 0, label %default [ i16 1, label %blockOne  i16 2, label %blockTwo ]")
  }
  
  test("switch with duplicate comparison constant fails") {
    val block = createTestBlock()

    val testValue = IntegerConstant(IntegerType(16), 0) 
    val defaultBlock = createTestBlock("default")
    val blockOne = createTestBlock("blockOne")
    val blockTwo = createTestBlock("blockTwo")

    intercept[InconsistentIrException] {
      block.switch(testValue, defaultBlock,
        (1L -> blockOne),
        (1L -> blockTwo)
      )
    }
  }
  
  test("trivial invoke") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="doNothing",
      arguments=declArgs)

    val normalBlock = createTestBlock("success")
    val exceptionBlock = createTestBlock("exception")

    val block = createTestBlock()
    val resultVar = block.invokeDecl(None)(
      decl=decl,
      arguments=List(),
      normalBlock=normalBlock,
      exceptionBlock=exceptionBlock
    )

    assert(resultVar === None)
    assertInstr(block, "invoke void @doNothing() to label %success unwind label %exception")
  }
  
  test("invoke returning value") {
    val declResult = IrFunction.Result(IntegerType(8), Set(IrFunction.ZeroExt))
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="returnSomething",
      arguments=declArgs)
    
    val normalBlock = createTestBlock("success")
    val exceptionBlock = createTestBlock("exception")

    val booleanRange = RangeMetadata(IntegerType(8), (0, 2))
    val callMetadata = Map(
      "range" -> booleanRange
    )

    val block = createTestBlock()
    val resultVar = block.invokeDecl(Some("ret"))(
      decl=decl,
      arguments=List(),
      normalBlock=normalBlock,
      exceptionBlock=exceptionBlock,
      metadata=callMetadata
    )

    assert(resultVar.isDefined)
    assertInstr(block, "%ret1 = invoke zeroext i8 @returnSomething() to label %success unwind label %exception, !range !{i8 0, i8 2}")
  }
  
  test("invoke discarding value") {
    val declResult = IrFunction.Result(IntegerType(8), Set(IrFunction.ZeroExt))
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="returnSomething",
      arguments=declArgs)
    
    val normalBlock = createTestBlock("success")
    val exceptionBlock = createTestBlock("exception")

    val block = createTestBlock()
    val resultVar = block.invokeDecl(None)(
      decl=decl,
      arguments=List(),
      normalBlock,
      exceptionBlock
    )

    assert(resultVar === None)
    assertInstr(block, "invoke zeroext i8 @returnSomething() to label %success unwind label %exception")
  }
  
  test("fastcc invoke") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="fastCc",
      arguments=declArgs,
      callingConv=CallingConv.FastCC)

    val normalBlock = createTestBlock("success")
    val exceptionBlock = createTestBlock("exception")

    val block = createTestBlock()
    val resultVar = block.invokeDecl(None)(
      decl=decl,
      arguments=List(),
      normalBlock,
      exceptionBlock
    )

    assert(resultVar === None)
    assertInstr(block, "invoke fastcc void @fastCc() to label %success unwind label %exception")
  }

  test("invoke with insufficent args") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)))
    val decl = IrFunctionDecl(
      result=declResult,
      name="notEnoughArgs",
      arguments=declArgs)
    
    val normalBlock = createTestBlock("success")
    val exceptionBlock = createTestBlock("exception")
    
    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.invokeDecl(None)(decl=decl, arguments=List(), normalBlock, exceptionBlock)
    }
  }
  
  test("invoke with insufficent varargs") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)))
    val decl = IrFunctionDecl(
      result=declResult,
      name="notEnoughArgs",
      arguments=declArgs,
      hasVararg=true)
    
    val normalBlock = createTestBlock("success")
    val exceptionBlock = createTestBlock("exception")
    
    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.invokeDecl(None)(decl=decl, arguments=List(), normalBlock, exceptionBlock)
    }
  }

  test("invoke with unmatched args") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(IrFunction.Argument(IntegerType(8), Set(IrFunction.NoCapture)))
    val decl = IrFunctionDecl(
      result=declResult,
      name="mismatchedArgs",
      arguments=declArgs)
    
    val normalBlock = createTestBlock("success")
    val exceptionBlock = createTestBlock("exception")
    
    val block = createTestBlock()
    val mismatchedValue = IntegerConstant(IntegerType(16), 5)

    intercept[InconsistentIrException] {
      block.invokeDecl(None)(
        decl=decl,
        arguments=List(mismatchedValue),
        normalBlock,
        exceptionBlock
      )
    }
  }
  
  test("christmas tree invoke") {
    val declResult = IrFunction.Result(IntegerType(8), Set(IrFunction.ZeroExt))
    val declArgs = List(
      IrFunction.Argument(FloatType, Set(IrFunction.NoCapture, IrFunction.NoAlias)),
      IrFunction.Argument(PointerType(DoubleType), Set())
    )
    val decl = IrFunctionDecl(
      result=declResult,
      name="uberCall",
      arguments=declArgs,
      attributes=Set(IrFunction.NoUnwind, IrFunction.NoReturn),
      callingConv=CallingConv.ColdCC
    )
    
    val normalBlock = createTestBlock("success")
    val exceptionBlock = createTestBlock("exception")
    
    val block = createTestBlock()
    val resultVar = block.invokeDecl(Some("ret"))(
      decl=decl,
      arguments=List(
        FloatConstant(2.0f),
        LocalVariable("local", PointerType(DoubleType))
      ),
      normalBlock=normalBlock,
      exceptionBlock=exceptionBlock
    )

    assert(resultVar.isDefined)
    assertInstr(block, "%ret1 = invoke coldcc zeroext i8 @uberCall(float 2.0, double* %local) noreturn nounwind to label %success unwind label %exception")
  }
}
