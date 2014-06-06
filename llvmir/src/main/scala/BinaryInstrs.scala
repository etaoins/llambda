package io.llambda.llvmir

sealed abstract class WrapBehaviour(val mnemonic : String)

object WrapBehaviour {
  case object NoSignedWrap extends WrapBehaviour("nsw")
  case object NoUnsignedWrap extends WrapBehaviour("nuw")
}

private[llvmir] trait BinaryInstrs extends IrInstrBuilder {
  private def simpleIntegerMathInstr(instruction : String)(resultDest : ResultDestination)(wrapBehavior : Set[WrapBehaviour], op1 : IrValue, op2 : IrValue) : IrValue = {
    if (op1.irType != op2.irType) {
      throw new InconsistentIrException(s"Attempted ${instruction} with non-identical types")
    }
    
    val resultType = op1.irType 
    
    resultType match {
      case IntegerType(_) =>
      case _ =>
        throw new InconsistentIrException(s"Attempted ${instruction} of non-integer")
    }
    
    val resultVar = resultDest.asLocalVariable(nameSource, resultType)

    val wrapIr = wrapBehavior.map(_.mnemonic + " ").toList.sorted.mkString("")
    addInstruction(s"${resultVar.toIr} = ${instruction} ${wrapIr}${resultType.toIr} ${op1.toIr}, ${op2.toIr}")

    resultVar
  }

  val add = simpleIntegerMathInstr("add")_
  val sub = simpleIntegerMathInstr("sub")_
  val mul = simpleIntegerMathInstr("mul")_
}
