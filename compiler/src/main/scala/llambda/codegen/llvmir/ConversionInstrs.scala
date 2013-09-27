package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

private[llvmir] trait ConversionInstrs extends IrInstrBuilder {
  def truncto(resultName : String)(value : IrValue, toType : IntegerType) : IrValue = {
    value.irType match {
      case IntegerType(fromBits) =>
        if (fromBits <= toType.bits) {
          throw new InternalCompilerErrorException(s"Attempted truncto from ${fromBits} to ${toType.bits}")
        }

      case _ => throw new InternalCompilerErrorException("Attempted truncto from non-integer")
    }

    val resultVar = allocateLocalVar(toType, resultName)
    instructions += s"${resultVar.toIr} = trunc ${value.toIrWithType} to ${toType.toIr}"
    
    resultVar
  }

  private def genericExttoInstr(instruction : String)(resultName : String)(value : IrValue, toType : IntegerType) : IrValue = {
    value.irType match {
      case IntegerType(fromBits) =>
        if (fromBits >= toType.bits) {
          throw new InternalCompilerErrorException(s"Attempted extcto from ${fromBits} to ${toType.bits}")
        }

      case _ => throw new InternalCompilerErrorException("Attempted extto from non-integer")
    }

    val resultVar = allocateLocalVar(toType, resultName)
    instructions += s"${resultVar.toIr} = ${instruction} ${value.toIrWithType} to ${toType.toIr}"

    resultVar
  }

  val sextto = genericExttoInstr("sext")_
  val zextto = genericExttoInstr("zext")_
  
  def bitcastto(resultName : String)(value : IrValue, toType : FirstClassType) : IrValue = {
    val resultVar = allocateLocalVar(toType, resultName)
    instructions += s"${resultVar.toIr} = bitcast ${value.toIrWithType} to ${toType.toIr}"

    resultVar
  }
}
