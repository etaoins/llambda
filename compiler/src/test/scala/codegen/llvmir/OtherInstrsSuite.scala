package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class OtherInstrsSuite extends FunSuite {
  test("sourceless ph") {
    implicit val nameSource = new LocalNameSource
    intercept[InternalCompilerErrorException] {
      new IrBlockBuilder {
        phi("error")()
      }
    }
  }

  test("incompatible source phi") {
    implicit val nameSource = new LocalNameSource
    intercept[InternalCompilerErrorException] {
      new IrBlockBuilder {
        phi("error")(
          PhiSource(IntegerConstant(IntegerType(1), 0), IrLabel("one")),
          PhiSource(SingleConstant(2.0f), IrLabel("two"))
        )
      }
    }
  }

  test("single source phi") {
    implicit val nameSource = new LocalNameSource
    val block = new IrBlockBuilder {
      val resultVar = phi("singlesource")(
        PhiSource(IntegerConstant(IntegerType(1), 0), IrLabel("one"))
      )

      assert(resultVar === LocalVariable("singlesource1", IntegerType(1)))
    }

    assert(block.toIr === "\t%singlesource1 = phi i1 [ 0, %one ]")
  }

  test("two source phi") {
    implicit val nameSource = new LocalNameSource
    val block = new IrBlockBuilder {
      val resultVar = phi("twosource")(
        PhiSource(DoubleConstant(1.0), IrLabel("plusone")),
        PhiSource(DoubleConstant(-2.0), IrLabel("minustwo"))
      )

      assert(resultVar === LocalVariable("twosource1", DoubleType))
    }

    assert(block.toIr === "\t%twosource1 = phi double [ 1.0, %plusone ], [ -2.0, %minustwo ]")
  }
  
  test("trivial call") {
    implicit val nameSource = new LocalNameSource
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="doNothing",
      arguments=declArgs)

    val block = new IrBlockBuilder {
      val resultVar = callDecl(None)(
        decl=decl,
        arguments=List()
      )

      assert(resultVar === None)
    }

    assert(block.toIr === "\tcall void @doNothing()")
  }
  
  test("call returning value") {
    implicit val nameSource = new LocalNameSource
    val declResult = IrFunction.Result(IntegerType(8), Set(IrFunction.ZeroExt))
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="returnSomething",
      arguments=declArgs)

    val block = new IrBlockBuilder {
      val resultVar = callDecl(Some("ret"))(
        decl=decl,
        arguments=List()
      )

      assert(resultVar.isDefined)
    }

    assert(block.toIr === "\t%ret1 = call zeroext i8 @returnSomething()")
  }
  
  test("call discarding value") {
    implicit val nameSource = new LocalNameSource
    val declResult = IrFunction.Result(IntegerType(8), Set(IrFunction.ZeroExt))
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="returnSomething",
      arguments=declArgs)

    val block = new IrBlockBuilder {
      val resultVar = callDecl(None)(
        decl=decl,
        arguments=List()
      )

      assert(resultVar === None)
    }

    assert(block.toIr === "\tcall zeroext i8 @returnSomething()")
  }
  
  test("fastcc call") {
    implicit val nameSource = new LocalNameSource
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="fastCc",
      arguments=declArgs,
      callingConv=CallingConv.FastCC)

    val block = new IrBlockBuilder {
      val resultVar = callDecl(None)(
        decl=decl,
        arguments=List()
      )

      assert(resultVar === None)
    }

    assert(block.toIr === "\tcall fastcc void @fastCc()")
  }
  
  test("tail call") {
    implicit val nameSource = new LocalNameSource
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="doNothing",
      arguments=declArgs)

    val block = new IrBlockBuilder {
      val resultVar = callDecl(None)(
        decl=decl,
        arguments=List(),
        tailCall=true
      )

      assert(resultVar === None)
    }

    assert(block.toIr === "\ttail call void @doNothing()")
  }

  test("call with insufficent args") {
    implicit val nameSource = new LocalNameSource
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)))
    val decl = IrFunctionDecl(
      result=declResult,
      name="notEnoughArgs",
      arguments=declArgs)
    
    val block = new IrBlockBuilder {
      intercept[InternalCompilerErrorException] {
        callDecl(None)(decl=decl, arguments=List())
      }
    }
  }

  test("call with unmatched args") {
    implicit val nameSource = new LocalNameSource
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(IrFunction.Argument(IntegerType(8), Set(IrFunction.NoCapture)))
    val decl = IrFunctionDecl(
      result=declResult,
      name="mismatchedArgs",
      arguments=declArgs)
    
    val block = new IrBlockBuilder {
      val mismatchedValue = IntegerConstant(IntegerType(16), 5)

      intercept[InternalCompilerErrorException] {
        callDecl(None)(
          decl=decl,
          arguments=List(mismatchedValue)
        )
      }
    }
  }

  test("call with args") {
    implicit val nameSource = new LocalNameSource
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(
      IrFunction.Argument(IntegerType(8), Set(IrFunction.NoCapture)),
      IrFunction.Argument(PointerType(IntegerType(1)), Set())
    )

    val decl = IrFunctionDecl(
      result=declResult,
      name="withArgs",
      arguments=declArgs)
    
    val block = new IrBlockBuilder {
      val mismatchedValue = IntegerConstant(IntegerType(16), 5)

      val resultVar = callDecl(None)(
        decl=decl,
        arguments=List(
          IntegerConstant(IntegerType(8), 1),
          LocalVariable("local", PointerType(IntegerType(1)))
        )
      )
    
      assert(resultVar === None)
    }
    
    assert(block.toIr === "\tcall void @withArgs(i8 1, i1* %local)")
  }
  
  test("call with attrs") {
    implicit val nameSource = new LocalNameSource
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="withAttrs",
      arguments=declArgs,
      // IrFunction.Cold should be filtered out
      attributes=Set(IrFunction.Cold, IrFunction.ReadOnly)
    )

    val block = new IrBlockBuilder {
      val resultVar = callDecl(None)(
        decl=decl,
        arguments=List()
      )

      assert(resultVar === None)
    }

    assert(block.toIr === "\tcall void @withAttrs() readonly")
  }

  test("christmas tree call") {
    implicit val nameSource = new LocalNameSource
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
    
    val block = new IrBlockBuilder {
      val resultVar = callDecl(Some("ret"))(
        decl=decl,
        arguments=List(
          SingleConstant(2.0f),
          LocalVariable("local", PointerType(DoubleType))
        ),
        tailCall=true
      )

      assert(resultVar.isDefined)
    }

    assert(block.toIr === "\t%ret1 = tail call coldcc zeroext i8 @uberCall(float 2.0, double* %local) noreturn nounwind")
  }
}

