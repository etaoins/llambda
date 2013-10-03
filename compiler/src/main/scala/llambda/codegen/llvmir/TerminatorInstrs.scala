package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

private[llvmir] trait TerminatorInstrs extends IrInstrBuilder {
  def ret(value : IrValue) {
    instructions += s"ret ${value.toIrWithType}"
  }

  def retVoid() {
    instructions += "ret void"
  }

  def condBranch(cond : IrValue, trueBlock : IrBranchTarget, falseBlock : IrBranchTarget) {
    if (cond.irType != IntegerType(1)) {
      throw new InternalCompilerErrorException("Attempted to branch using non-i1")
    }

    instructions += s"br ${cond.toIrWithType}, label %${trueBlock.label}, label %${falseBlock.label}"
  }

  def uncondBranch(block : IrBranchTarget) {
    instructions += s"br label %${block.label}"
  }

  def unreachable() {
    instructions += "unreachable"
  }
}

