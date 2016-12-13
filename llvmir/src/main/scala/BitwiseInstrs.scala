package io.llambda.llvmir

private[llvmir] trait BitwiseInstrs extends IrInstrBuilder {
  private def simpleBitwiseInstr(instruction: String)(resultDest: ResultDestination)(op1: IrValue, op2: IrValue): IrValue = {
    if (op1.irType != op2.irType) {
      throw new InconsistentIrException("Attempted bitwise operation with non-identical types")
    }

    val resultType = op1.irType

    resultType match {
      case IntegerType(_) =>
      case _ =>
        throw new InconsistentIrException("Attempted bitwise operation of non-integer")
    }

    val resultVar = resultDest.asLocalVariable(nameSource, resultType)
    addInstruction(s"${resultVar.toIr} = ${instruction} ${resultType.toIr} ${op1.toIr}, ${op2.toIr}")

    resultVar
  }

  val and = simpleBitwiseInstr("and")_
  val or  = simpleBitwiseInstr("or")_
  val xor = simpleBitwiseInstr("xor")_
}
