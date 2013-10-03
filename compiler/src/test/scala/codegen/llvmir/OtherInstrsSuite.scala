package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

class OtherInstrsSuite extends IrTestSuite {
  test("integer equality") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()
    val resultVal = block.icmp("equal")(ComparisonCond.Equal, None, var1, var2)

    assertInstr(block, "%equal1 = icmp eq i32 20, 30")
  }
  
  test("integer inequality") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()
    val resultVal = block.icmp("inequal")(ComparisonCond.NotEqual, None, var1, var2)

    assertInstr(block, "%inequal1 = icmp ne i32 20, 30")
  }
  
  test("integer signed greater than") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()
    val resultVal = block.icmp("greaterthan")(ComparisonCond.GreaterThan, Some(true), var1, var2)

    assertInstr(block, "%greaterthan1 = icmp sgt i32 20, 30")
  }
  
  test("integer unsigned less than equals") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()
    val resultVal = block.icmp("lessthanequals")(ComparisonCond.LessThanEqual, Some(false), var1, var2)

    assertInstr(block, "%lessthanequals1 = icmp ule i32 20, 30")
  }
  
  test("integer equality with signedness fails") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.icmp("signed")(ComparisonCond.Equal, Some(true), var1, var2)
    }
  }
  
  test("integer less than without signedness fails") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.icmp("notsigned")(ComparisonCond.LessThan, None, var1, var2)
    }
  }
  
  test("pointer equality") {
    val fakePointer1 = LocalVariable("fake1", PointerType(IntegerType(64)))
    val fakePointer2 = LocalVariable("fake2", PointerType(IntegerType(64)))

    val block = createTestBlock()
    val resultVal = block.icmp("pointer")(ComparisonCond.Equal, None, fakePointer1, fakePointer2)

    assertInstr(block, "%pointer1 = icmp eq i64* %fake1, %fake2")
  }
  
  test("integer equality with incompatible types fails") {
    val var1 = IntegerConstant(IntegerType(64), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.icmp("incompatible")(ComparisonCond.Equal, None, var1, var2)
    }
  }
  
  test("integer equality with doubles fails") {
    val var1 = DoubleConstant(20.0)
    val var2 = DoubleConstant(30.0)

    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.icmp("noninteger")(ComparisonCond.Equal, None, var1, var2)
    }
  }

  test("trivial call") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="doNothing",
      arguments=declArgs)

    val block = createTestBlock()
    val resultVar = block.callDecl(None)(
      decl=decl,
      arguments=List()
    )

    assert(resultVar === None)
    assertInstr(block, "call void @doNothing()")
  }
  
  test("call returning value") {
    val declResult = IrFunction.Result(IntegerType(8), Set(IrFunction.ZeroExt))
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="returnSomething",
      arguments=declArgs)

    val block = createTestBlock()
    val resultVar = block.callDecl(Some("ret"))(
      decl=decl,
      arguments=List()
    )

    assert(resultVar.isDefined)
    assertInstr(block, "%ret1 = call zeroext i8 @returnSomething()")
  }
  
  test("call discarding value") {
    val declResult = IrFunction.Result(IntegerType(8), Set(IrFunction.ZeroExt))
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="returnSomething",
      arguments=declArgs)

    val block = createTestBlock()
    val resultVar = block.callDecl(None)(
      decl=decl,
      arguments=List()
    )

    assert(resultVar === None)
    assertInstr(block, "call zeroext i8 @returnSomething()")
  }
  
  test("fastcc call") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="fastCc",
      arguments=declArgs,
      callingConv=CallingConv.FastCC)

    val block = createTestBlock()
    val resultVar = block.callDecl(None)(
      decl=decl,
      arguments=List()
    )

    assert(resultVar === None)
    assertInstr(block, "call fastcc void @fastCc()")
  }
  
  test("tail call") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="doNothing",
      arguments=declArgs)

    val block = createTestBlock()
    val resultVar = block.callDecl(None)(
      decl=decl,
      arguments=List(),
      tailCall=true
    )

    assert(resultVar === None)
    assertInstr(block, "tail call void @doNothing()")
  }

  test("call with insufficent args") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)))
    val decl = IrFunctionDecl(
      result=declResult,
      name="notEnoughArgs",
      arguments=declArgs)
    
    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.callDecl(None)(decl=decl, arguments=List())
    }
  }

  test("call with unmatched args") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(IrFunction.Argument(IntegerType(8), Set(IrFunction.NoCapture)))
    val decl = IrFunctionDecl(
      result=declResult,
      name="mismatchedArgs",
      arguments=declArgs)
    
    val block = createTestBlock()
    val mismatchedValue = IntegerConstant(IntegerType(16), 5)

    intercept[InternalCompilerErrorException] {
      block.callDecl(None)(
        decl=decl,
        arguments=List(mismatchedValue)
      )
    }
  }

  test("call with args") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(
      IrFunction.Argument(IntegerType(8), Set(IrFunction.NoCapture)),
      IrFunction.Argument(PointerType(IntegerType(1)), Set())
    )

    val decl = IrFunctionDecl(
      result=declResult,
      name="withArgs",
      arguments=declArgs)
    
    val block = createTestBlock()
    val mismatchedValue = IntegerConstant(IntegerType(16), 5)

    val resultVar = block.callDecl(None)(
      decl=decl,
      arguments=List(
        IntegerConstant(IntegerType(8), 1),
        LocalVariable("local", PointerType(IntegerType(1)))
      )
    )
  
    assert(resultVar === None)
    assertInstr(block, "call void @withArgs(i8 1, i1* %local)")
  }
  
  test("call with attrs") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="withAttrs",
      arguments=declArgs,
      // IrFunction.Cold should be filtered out
      attributes=Set(IrFunction.Cold, IrFunction.ReadOnly)
    )

    val block = createTestBlock()
    val resultVar = block.callDecl(None)(
      decl=decl,
      arguments=List()
    )

    assert(resultVar === None)
    assertInstr(block, "call void @withAttrs() readonly")
  }

  test("christmas tree call") {
    val declResult = IrFunction.Result(IntegerType(8), Set(IrFunction.ZeroExt))
    val declArgs = List(
      IrFunction.Argument(SingleType, Set(IrFunction.NoCapture, IrFunction.NoAlias)),
      IrFunction.Argument(PointerType(DoubleType), Set())
    )
    val decl = IrFunctionDecl(
      result=declResult,
      name="uberCall",
      arguments=declArgs,
      attributes=Set(IrFunction.NoUnwind, IrFunction.NoReturn),
      callingConv=CallingConv.ColdCC
    )
    
    val block = createTestBlock()
    val resultVar = block.callDecl(Some("ret"))(
      decl=decl,
      arguments=List(
        SingleConstant(2.0f),
        LocalVariable("local", PointerType(DoubleType))
      ),
      tailCall=true
    )

    assert(resultVar.isDefined)
    assertInstr(block, "%ret1 = tail call coldcc zeroext i8 @uberCall(float 2.0, double* %local) noreturn nounwind")
  }
}

