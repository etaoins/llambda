package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

private[llvmir] trait TerminatorInstrs extends IrInstrBuilder {
  def ret(value : IrValue) {
    instructions += s"ret ${value.toIrWithType}"
  }

  def retVoid() {
    instructions += "ret void"
  }

  def condBranch(cond : IrValue, trueLabel : IrLabel, falseLabel : IrLabel) {
    if (cond.irType != IntegerType(1)) {
      throw new InternalCompilerErrorException("Attempted to branch using non-i1")
    }

    instructions += s"br ${cond.toIrWithType}, label ${trueLabel.toIr}, label ${falseLabel.toIr}"
  }

  def uncondBranch(label : IrLabel) {
    instructions += s"br label ${label.toIr}"
  }

  def unreachable() {
    instructions += "unreachable"
  }
}

