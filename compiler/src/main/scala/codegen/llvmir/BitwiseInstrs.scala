package io.llambda.compiler.codegen.llvmir
import io.llambda

import llambda.compiler.InternalCompilerErrorException

private[llvmir] trait BitwiseInstrs extends IrInstrBuilder {
  private def simpleBitwiseInstr(instruction : String)(resultName : String)(op1 : IrValue, op2 : IrValue) : IrValue = {
    if (op1.irType != op2.irType) {
      throw new InternalCompilerErrorException("Attempted bitwise operation with non-identical types")
    }

    val resultType = op1.irType 

    resultType match {
      case IntegerType(_) =>
      case _ =>
        throw new InternalCompilerErrorException("Attempted bitwise operation of non-integer")
    }

    val resultVar = allocateLocalVar(resultType, resultName)
    instructions += s"${resultVar.toIr} = ${instruction} ${resultType.toIr} ${op1.toIr}, ${op2.toIr}" 

    resultVar
  }

  val and = simpleBitwiseInstr("and")_
  val or  = simpleBitwiseInstr("or")_
  val xor = simpleBitwiseInstr("xor")_
}
