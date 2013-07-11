package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class TerminatorInstrsSuite extends FunSuite {
  test("ret with value") {
    val block = new IrBlock {
      ret(IntegerConstant(IntegerType(16), 45))
    }

    assert(block.toIr === "\tret i16 45")
  }
  
  test("ret without value") {
    val block = new IrBlock {
      retVoid()
    }

    assert(block.toIr === "\tret void")
  }

  test("valid conditional branch") {
    val block = new IrBlock {
      condBranch(IntegerConstant(IntegerType(1), 0), IrLabel("true"), IrLabel("false"))
    }

    assert(block.toIr === "\tbr i1 0, label %true, label %false")
  }

  test("conditional branch with bad cond") {
    intercept[InternalCompilerErrorException] {
      new IrBlock {
        condBranch(StringConstant("Hello, world!"), IrLabel("true"), IrLabel("false"))
      }
    }
  }

  test("unconditional branch") {
    val block = new IrBlock {
      uncondBranch(IrLabel("alwayshere"))
    }

    assert(block.toIr === "\tbr label %alwayshere")
  }

  test("unreachable") {
    val block = new IrBlock {
      unreachable()
    }

    assert(block.toIr === "\tunreachable")
  }
}
