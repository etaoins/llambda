package io.llambda.llvmir

class OtherInstrsSuite extends IrTestSuite {
  private val gxxResultType = StructureType(List(
    PointerType(IntegerType(8)),
    IntegerType(32)
  ))

  test("integer equality") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()
    val resultVal = block.icmp("equal")(IComparisonCond.Equal, None, var1, var2)

    assert(resultVal.irType === IntegerType(1))
    assertInstr(block, "%equal1 = icmp eq i32 20, 30")
  }
  
  test("integer inequality") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()
    val resultVal = block.icmp("inequal")(IComparisonCond.NotEqual, None, var1, var2)

    assert(resultVal.irType === IntegerType(1))
    assertInstr(block, "%inequal1 = icmp ne i32 20, 30")
  }
  
  test("integer signed greater than") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()
    val resultVal = block.icmp("greaterthan")(IComparisonCond.GreaterThan, Some(true), var1, var2)

    assert(resultVal.irType === IntegerType(1))
    assertInstr(block, "%greaterthan1 = icmp sgt i32 20, 30")
  }
  
  test("integer unsigned less than equals") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()
    val resultVal = block.icmp("lessthanequals")(IComparisonCond.LessThanEqual, Some(false), var1, var2)

    assert(resultVal.irType === IntegerType(1))
    assertInstr(block, "%lessthanequals1 = icmp ule i32 20, 30")
  }
  
  test("integer equality with signedness fails") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.icmp("signed")(IComparisonCond.Equal, Some(true), var1, var2)
    }
  }
  
  test("integer less than without signedness fails") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.icmp("notsigned")(IComparisonCond.LessThan, None, var1, var2)
    }
  }
  
  test("pointer equality") {
    val fakePointer1 = LocalVariable("fake1", PointerType(IntegerType(64)))
    val fakePointer2 = LocalVariable("fake2", PointerType(IntegerType(64)))

    val block = createTestBlock()
    val resultVal = block.icmp("pointer")(IComparisonCond.Equal, None, fakePointer1, fakePointer2)

    assert(resultVal.irType === IntegerType(1))
    assertInstr(block, "%pointer1 = icmp eq i64* %fake1, %fake2")
  }
  
  test("integer equality with incompatible types fails") {
    val var1 = IntegerConstant(IntegerType(64), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.icmp("incompatible")(IComparisonCond.Equal, None, var1, var2)
    }
  }
  
  test("integer equality with doubles fails") {
    val var1 = DoubleConstant(20.0)
    val var2 = DoubleConstant(30.0)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.icmp("noninteger")(IComparisonCond.Equal, None, var1, var2)
    }
  }

  test("float ordered equality") {
    val var1 = DoubleConstant(20.0)
    val var2 = DoubleConstant(30.0)

    val block = createTestBlock()
    val resultVal = block.fcmp("equal")(FComparisonCond.OrderedEqual, var1, var2)

    assert(resultVal.irType === IntegerType(1))
    assertInstr(block, "%equal1 = fcmp oeq double 20.0, 30.0")
  }
  
  test("float unordered inequality") {
    val var1 = FloatConstant(20.0f)
    val var2 = FloatConstant(30.0f)

    val block = createTestBlock()
    val resultVal = block.fcmp("inequal")(FComparisonCond.UnorderedNotEqual, var1, var2)

    assert(resultVal.irType === IntegerType(1))
    assertInstr(block, "%inequal1 = fcmp une float 20.0, 30.0")
  }
  
  test("float equality with incompatible types fails") {
    val var1 = FloatConstant(20.0f)
    val var2 = DoubleConstant(30.0f)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.fcmp("incompatible")(FComparisonCond.Ordered, var1, var2)
    }
  }
  
  test("float equality with integers fails") {
    val var1 = IntegerConstant(IntegerType(32), 20)
    val var2 = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.fcmp("nonfloat")(FComparisonCond.True, var1, var2)
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

    val booleanRange = RangeMetadata(IntegerType(8), (0, 2))
    val callMetadata = Map(
      "range" -> booleanRange
    )

    val block = createTestBlock()
    val resultVar = block.callDecl(Some("ret"))(
      decl=decl,
      arguments=List(),
      metadata=callMetadata
    )

    assert(resultVar.isDefined)
    assertInstr(block, "%ret1 = call zeroext i8 @returnSomething(), !range !{i8 0, i8 2}")
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

    intercept[InconsistentIrException] {
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

    intercept[InconsistentIrException] {
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
  
  test("call with varargs") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(
      IrFunction.Argument(IntegerType(8), Set(IrFunction.NoCapture)),
      IrFunction.Argument(PointerType(IntegerType(1)), Set())
    )

    val decl = IrFunctionDecl(
      result=declResult,
      name="withArgs",
      arguments=declArgs,
      hasVararg=true
    )
    
    val block = createTestBlock()

    val resultVar = block.callDecl(None)(
      decl=decl,
      arguments=List(
        IntegerConstant(IntegerType(8), 1),
        LocalVariable("local", PointerType(IntegerType(1))),
        DoubleConstant(5.0)
      )
    )
  
    assert(resultVar === None)
    assertInstr(block, "call void @withArgs(i8 1, i1* %local, double 5.0)")
  }
  
  test("call with too many args") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(
      IrFunction.Argument(IntegerType(8), Set(IrFunction.NoCapture)),
      IrFunction.Argument(PointerType(IntegerType(1)), Set())
    )

    val decl = IrFunctionDecl(
      result=declResult,
      name="withArgs",
      arguments=declArgs
    )
    
    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.callDecl(None)(
        decl=decl,
        arguments=List(
          IntegerConstant(IntegerType(8), 1),
          LocalVariable("local", PointerType(IntegerType(1))),
          DoubleConstant(5.0)
        )
      )
    }
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
    
    val block = createTestBlock()
    val resultVar = block.callDecl(Some("ret"))(
      decl=decl,
      arguments=List(
        FloatConstant(2.0f),
        LocalVariable("local", PointerType(DoubleType))
      ),
      tailCall=true
    )

    assert(resultVar.isDefined)
    assertInstr(block, "%ret1 = tail call coldcc zeroext i8 @uberCall(float 2.0, double* %local) noreturn nounwind")
  }
  
  test("trivial select") {
    val condValue = IntegerConstant(IntegerType(1), 1) 
    val trueValue = IntegerConstant(IntegerType(32), 20)
    val falseValue = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()
    val resultVal = block.select("trivial")(condValue, trueValue, falseValue)

    assert(resultVal.irType === IntegerType(32))
    assertInstr(block, "%trivial1 = select i1 1, i32 20, i32 30")
  }

  test("select with non-boolean") {
    val condValue = DoubleConstant(1.0)
    val trueValue = IntegerConstant(IntegerType(32), 20)
    val falseValue = IntegerConstant(IntegerType(32), 30)

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.select("nonbool")(condValue, trueValue, falseValue)
    }
  }
  
  test("select with incompatible types") {
    val condValue = IntegerConstant(IntegerType(1), 1) 
    val trueValue = IntegerConstant(IntegerType(32), 20)
    val falseValue = IntegerConstant(IntegerType(64), 30)

    val block = createTestBlock()
    
    intercept[InconsistentIrException] {
      block.select("incompattypes")(condValue, trueValue, falseValue)
    }
  }

  test("resume with i32 5") {
    val block = createTestBlock()
    block.resume(IntegerConstant(IntegerType(32), 5))

    assertInstr(block, "resume i32 5")
  }

  test("non-cleanup landingpad with no clauses fails") {
    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.landingpad("nocleanup")(gxxResultType, Nil)
    }
  }
  
  test("cleanup landingpad with no clauses") {
    val block = createTestBlock()

    val resultVal = block.landingpad("cleanup")(gxxResultType, Nil, true)

    assert(resultVal.irType === gxxResultType)
    assertInstr(block, "%cleanup1 = landingpad {i8*, i32} cleanup")
  }
  
  test("non-cleanup landingpad with two catches") {
    val exceptionClass1 = GlobalVariable("class1", PointerType(PointerType(IntegerType(8))))
    val exceptionClass2 = GlobalVariable("class2", PointerType(PointerType(IntegerType(8))))

    val block = createTestBlock()

    val clauses = List(
      CatchClause(exceptionClass1),
      CatchClause(exceptionClass2)
    )

    val resultVal = block.landingpad("landresult")(gxxResultType, clauses, false)

    assert(resultVal.irType === gxxResultType)
    assertInstr(block,
      "%landresult1 = landingpad {i8*, i32} catch i8** @class1 catch i8** @class2"
    )
  }
  
  test("cleanup landingpad with catch and filter") {
    val exceptionClass1 = GlobalVariable("class1", PointerType(PointerType(IntegerType(8))))
    val exceptionClass2 = GlobalVariable("class2", PointerType(PointerType(IntegerType(8))))
    val exceptionClass3 = GlobalVariable("class3", PointerType(PointerType(IntegerType(8))))

    val block = createTestBlock()

    val clauses = List(
      CatchClause(exceptionClass1),
      FilterClause(
        PointerType(PointerType(IntegerType(8))),
        List(exceptionClass2, exceptionClass3)
      )
    )

    val resultVal = block.landingpad("landresult")(gxxResultType, clauses, true)

    assert(resultVal.irType === gxxResultType)
    assertInstr(block,
      "%landresult1 = landingpad {i8*, i32} cleanup catch i8** @class1 filter [2 x i8**] [i8** @class2, i8** @class3]"
    )
  }
}

