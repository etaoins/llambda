package io.llambda.llvmir

sealed abstract class WrapBehaviour(val mnemonic: String)

object WrapBehaviour {
  case object NoSignedWrap extends WrapBehaviour("nsw")
  case object NoUnsignedWrap extends WrapBehaviour("nuw")
}

sealed abstract class FastMathFlag(val mnemonic: String)

object FastMathFlag {
  case object NoNaN extends FastMathFlag("nnan")
  case object NoInf extends FastMathFlag("ninf")
  case object NoSignedZero extends FastMathFlag("nsz")
  case object AllowReciprocal extends FastMathFlag("arcp")
  case object Fast extends FastMathFlag("fast")
}

private[llvmir] trait BinaryInstrs extends IrInstrBuilder {
  private def commonIntegerType(instruction: String)(op1: IrValue, op2: IrValue): IntegerType = {
    if (op1.irType != op2.irType) {
      throw new InconsistentIrException(s"Attempted ${instruction} with non-identical types")
    }

    op1.irType match {
      case intType: IntegerType =>
        intType
      case _ =>
        throw new InconsistentIrException(s"Attempted ${instruction} of non-integer")
    }
  }

  private def simpleIntegerMathInstr(instruction: String)(resultDest: ResultDestination)(wrapBehavior: Set[WrapBehaviour], op1: IrValue, op2: IrValue): IrValue = {
    val resultType = commonIntegerType(instruction)(op1, op2)
    val resultVar = resultDest.asLocalVariable(nameSource, resultType)

    val wrapIr = wrapBehavior.map(_.mnemonic + " ").toList.sorted.mkString("")
    addInstruction(s"${resultVar.toIr} = ${instruction} ${wrapIr}${resultType.toIr} ${op1.toIr}, ${op2.toIr}")

    resultVar
  }

  private def simpleFloatMathInstr(instruction: String)(resultDest: ResultDestination)(fastMathFlags: Set[FastMathFlag], op1: IrValue, op2: IrValue): IrValue = {
    if (op1.irType != op2.irType) {
      throw new InconsistentIrException(s"Attempted ${instruction} with non-identical types")
    }

    val resultType = op1.irType

    resultType match {
      case _: FloatingPointType =>
      case _ =>
        throw new InconsistentIrException(s"Attempted ${instruction} of non-integer")
    }

    val resultVar = resultDest.asLocalVariable(nameSource, resultType)

    val fastMathIr = fastMathFlags.toList.map(_.mnemonic).sorted.map(_ + " ").mkString("")
    addInstruction(s"${resultVar.toIr} = ${instruction} ${fastMathIr}${resultType.toIr} ${op1.toIr}, ${op2.toIr}")

    resultVar
  }

  private def integerDivisionInstr(instruction: String)(resultDest: ResultDestination)(
      exact: Boolean,
      op1: IrValue,
      op2: IrValue
  ): IrValue = {
    val resultType = commonIntegerType(instruction)(op1, op2)
    val resultVar = resultDest.asLocalVariable(nameSource, resultType)

    val exactIr = if (exact) "exact " else ""

    addInstruction(s"${resultVar.toIr} = ${instruction} ${exactIr}${resultType.toIr} ${op1.toIr}, ${op2.toIr}")

    resultVar
  }

  private def integerRemainderInstr(instruction: String)(resultDest: ResultDestination)(
      op1: IrValue,
      op2: IrValue
  ): IrValue = {
    val resultType = commonIntegerType(instruction)(op1, op2)
    val resultVar = resultDest.asLocalVariable(nameSource, resultType)

    addInstruction(s"${resultVar.toIr} = ${instruction} ${resultType.toIr} ${op1.toIr}, ${op2.toIr}")

    resultVar
  }

  val add = simpleIntegerMathInstr("add")_
  val sub = simpleIntegerMathInstr("sub")_
  val mul = simpleIntegerMathInstr("mul")_
  val sdiv = integerDivisionInstr("sdiv")_
  val udiv = integerDivisionInstr("udiv")_
  val srem = integerRemainderInstr("srem")_
  val urem = integerRemainderInstr("urem")_

  val fadd = simpleFloatMathInstr("fadd")_
  val fsub = simpleFloatMathInstr("fsub")_
  val fmul = simpleFloatMathInstr("fmul")_
  val fdiv = simpleFloatMathInstr("fdiv")_
}
