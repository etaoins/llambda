package io.llambda.compiler.codegen.llvmir
import io.llambda

import llambda.compiler.InternalCompilerErrorException

class PhiInstrSuite extends IrTestSuite {
  test("sourceless phi") {
    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.phi("error")()
    }
  }

  test("incompatible source phi") {
    val block = createTestBlock()

    intercept[InternalCompilerErrorException] {
      block.phi("error")(
        PhiSource(IntegerConstant(IntegerType(1), 0), createTestBlock("one")),
        PhiSource(FloatConstant(2.0f), createTestBlock("two"))
      )
    }
  }

  test("single source phi") {
    val block = createTestBlock()
    val resultVar = block.phi("singlesource")(
      PhiSource(IntegerConstant(IntegerType(1), 0), createTestBlock("one"))
    )

    assert(resultVar === LocalVariable("singlesource1", IntegerType(1)))
    assertInstr(block, "%singlesource1 = phi i1 [ 0, %one ]")
  }

  test("two source phi") {
    val block = createTestBlock()
    val resultVar = block.phi("twosource")(
      PhiSource(DoubleConstant(1.0), createTestBlock("plusone")),
      PhiSource(DoubleConstant(-2.0), createTestBlock("minustwo"))
    )

    assert(resultVar === LocalVariable("twosource1", DoubleType))
    assertInstr(block, "%twosource1 = phi double [ 1.0, %plusone ], [ -2.0, %minustwo ]")
  }
  
}
