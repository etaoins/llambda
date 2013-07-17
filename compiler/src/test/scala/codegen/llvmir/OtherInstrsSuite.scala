package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class OtherInstrsSuite extends FunSuite {
  test("sourceless ph") {
    intercept[InternalCompilerErrorException] {
      new IrBlock {
        phi()
      }
    }
  }

  test("incompatible source phi") {
    intercept[InternalCompilerErrorException] {
      new IrBlock {
        phi(
          PhiSource(IntegerConstant(IntegerType(1), 0), IrLabel("one")),
          PhiSource(SingleConstant(2.0f), IrLabel("two"))
        )
      }
    }
  }

  test("single source phi") {
    val block = new IrBlock {
      val resultVar = phi(
        PhiSource(IntegerConstant(IntegerType(1), 0), IrLabel("one"))
      )

      assert(resultVar === LocalVariable("1", IntegerType(1)))
    }

    assert(block.toIr === "\t%1 = phi i1 [ 0, %one ]")
  }

  test("two source phi") {
    val block = new IrBlock {
      val resultVar = phi(
        PhiSource(DoubleConstant(1.0), IrLabel("plusone")),
        PhiSource(DoubleConstant(-2.0), IrLabel("minustwo"))
      )

      assert(resultVar === LocalVariable("1", DoubleType))
    }

    assert(block.toIr === "\t%1 = phi double [ 1.0, %plusone ], [ -2.0, %minustwo ]")
  }
  
  test("trivial call") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="doNothing",
      arguments=declArgs)

    val block = new IrBlock {
      val resultVar = callDecl(
        decl=decl,
        arguments=List()
      )

      assert(resultVar === None)
    }

    assert(block.toIr === "\tcall void @doNothing()")
  }
  
  test("call returning value") {
    val declResult = IrFunction.Result(IntegerType(8), Set(IrFunction.ZeroExt))
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="returnSomething",
      arguments=declArgs)

    val block = new IrBlock {
      val resultVar = callDecl(
        decl=decl,
        arguments=List()
      )

      assert(resultVar.isDefined)
    }

    assert(block.toIr === "\t%1 = call zeroext i8 @returnSomething()")
  }
  
  test("fastcc call") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="fastCc",
      arguments=declArgs,
      callingConv=CallingConv.FastCC)

    val block = new IrBlock {
      val resultVar = callDecl(
        decl=decl,
        arguments=List()
      )

      assert(resultVar === None)
    }

    assert(block.toIr === "\tcall fastcc void @fastCc()")
  }
  
  test("tail call") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List()
    val decl = IrFunctionDecl(
      result=declResult,
      name="doNothing",
      arguments=declArgs)

    val block = new IrBlock {
      val resultVar = callDecl(
        decl=decl,
        arguments=List(),
        tailCall=true
      )

      assert(resultVar === None)
    }

    assert(block.toIr === "\ttail call void @doNothing()")
  }

  test("call with insufficent args") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)))
    val decl = IrFunctionDecl(
      result=declResult,
      name="notEnoughArgs",
      arguments=declArgs)
    
    val block = new IrBlock {
      intercept[InternalCompilerErrorException] {
        callDecl(decl=decl, arguments=List())
      }
    }
  }

  test("call with unmatched args") {
    val declResult = IrFunction.Result(VoidType, Set())
    val declArgs = List(IrFunction.Argument(IntegerType(8), Set(IrFunction.NoCapture)))
    val decl = IrFunctionDecl(
      result=declResult,
      name="mismatchedArgs",
      arguments=declArgs)
    
    val block = new IrBlock {
      val mismatchedValue = IntegerConstant(IntegerType(16), 5)

      intercept[InternalCompilerErrorException] {
        callDecl(
          decl=decl,
          arguments=List(mismatchedValue))
      }
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
    
    val block = new IrBlock {
      val mismatchedValue = IntegerConstant(IntegerType(16), 5)

      val resultVar = callDecl(
        decl=decl,
        arguments=List(
          IntegerConstant(IntegerType(8), 1),
          LocalVariable("local", PointerType(IntegerType(1)))
        )
      )
    
      assert(resultVar === None)
    }
    
    assert(block.toIr === "\tcall void @withArgs(i8 1 nocapture, i1* %local)")
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

    val block = new IrBlock {
      val resultVar = callDecl(
        decl=decl,
        arguments=List()
      )

      assert(resultVar === None)
    }

    assert(block.toIr === "\tcall void @withAttrs() readonly")
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
    
    val block = new IrBlock {
      val resultVar = callDecl(
        decl=decl,
        arguments=List(
          SingleConstant(2.0f),
          LocalVariable("local", PointerType(DoubleType))
        ),
        tailCall=true
      )

      assert(resultVar.isDefined)
    }

    assert(block.toIr === "\t%1 = tail call coldcc zeroext i8 @uberCall(float 2.0 noalias nocapture, double* %local) noreturn nounwind")
  }
}

