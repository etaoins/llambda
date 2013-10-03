package llambda.codegen.llvmir

import org.scalatest.FunSuite

class IrTestSuite extends FunSuite  {
  def createTestBlock(label : String = "test") : IrChildBlockBuilder = 
    new IrChildBlockBuilder(new LocalNameSource, label)
  
  def assertInstrs(block : IrBlockBuilder, instrs : List[String]) {
    assert(block.instructions.toList === instrs)
  }

  def assertInstr(block : IrBlockBuilder, instr : String) {
    assertInstrs(block, List(instr))
  }
}
