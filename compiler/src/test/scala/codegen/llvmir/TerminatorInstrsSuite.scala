package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class TerminatorInstrsSuite extends FunSuite {
  test("ret with value") {
    val block = new IrBlockBuilder()(new LocalNameSource) {
      ret(IntegerConstant(IntegerType(16), 45))
    }

    assert(block.toIr === "\tret i16 45")
  }
  
  test("ret without value") {
    val block = new IrBlockBuilder()(new LocalNameSource) {
      retVoid()
    }

    assert(block.toIr === "\tret void")
  }

  test("valid conditional branch") {
    val block = new IrBlockBuilder()(new LocalNameSource) {
      condBranch(IntegerConstant(IntegerType(1), 0), IrLabel("true"), IrLabel("false"))
    }

    assert(block.toIr === "\tbr i1 0, label %true, label %false")
  }

  test("conditional branch with bad cond") {
    intercept[InternalCompilerErrorException] {
      new IrBlockBuilder()(new LocalNameSource) {
        condBranch(StringConstant("Hello, world!"), IrLabel("true"), IrLabel("false"))
      }
    }
  }

  test("unconditional branch") {
    val block = new IrBlockBuilder()(new LocalNameSource) {
      uncondBranch(IrLabel("alwayshere"))
    }

    assert(block.toIr === "\tbr label %alwayshere")
  }

  test("unreachable") {
    val block = new IrBlockBuilder()(new LocalNameSource) {
      unreachable()
    }

    assert(block.toIr === "\tunreachable")
  }
}
