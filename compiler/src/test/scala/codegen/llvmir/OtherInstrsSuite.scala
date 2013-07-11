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
}

