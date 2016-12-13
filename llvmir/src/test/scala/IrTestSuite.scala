package io.llambda.llvmir

import org.scalatest.FunSuite

class IrTestSuite extends FunSuite  {
  def createTestModule(): IrModuleBuilder =
    new IrModuleBuilder

  def createTestFunction() = new IrFunctionBuilder(
    module=createTestModule(),
    result=IrFunction.Result(IntegerType(32), Set()),
    name="fake",
    namedArguments=Nil
  )

  def createTestBlock(label: String = "test"): IrChildBlockBuilder =
    new IrChildBlockBuilder(createTestFunction(), new LocalNameSource, label)

  def assertInstrs(block: IrBlockBuilder, instrs: List[String]) {
    assert(block.irLines.toList === instrs)
  }

  def assertInstr(block: IrBlockBuilder, instr: String) {
    assertInstrs(block, List(instr))
  }
}
