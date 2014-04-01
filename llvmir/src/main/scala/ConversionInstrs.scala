package io.llambda.llvmir

private[llvmir] trait ConversionInstrs extends IrInstrBuilder {
  def truncTo(resultDest : ResultDestination)(value : IrValue, toType : IntegerType) : IrValue = {
    value.irType match {
      case IntegerType(fromBits) =>
        if (fromBits <= toType.bits) {
          throw new InconsistentIrException(s"Attempted truncto from ${fromBits} to ${toType.bits}")
        }

      case _ => throw new InconsistentIrException("Attempted truncto from non-integer")
    }

    val resultVar = resultDest.asLocalVariable(nameSource, toType)
    instructions += s"${resultVar.toIr} = trunc ${value.toIrWithType} to ${toType.toIr}"
    
    resultVar
  }

  private def genericExtToInstr(instruction : String)(resultDest : ResultDestination)(value : IrValue, toType : IntegerType) : IrValue = {
    value.irType match {
      case IntegerType(fromBits) =>
        if (fromBits >= toType.bits) {
          throw new InconsistentIrException(s"Attempted extto from ${fromBits} to ${toType.bits}")
        }

      case _ => throw new InconsistentIrException("Attempted extto from non-integer")
    }

    val resultVar = resultDest.asLocalVariable(nameSource, toType)
    instructions += s"${resultVar.toIr} = ${instruction} ${value.toIrWithType} to ${toType.toIr}"

    resultVar
  }

  val sextTo = genericExtToInstr("sext")_
  val zextTo = genericExtToInstr("zext")_
  
  def bitcastTo(resultDest : ResultDestination)(value : IrValue, toType : FirstClassType) : IrValue = {
    val resultVar = resultDest.asLocalVariable(nameSource, toType)
    instructions += s"${resultVar.toIr} = bitcast ${value.toIrWithType} to ${toType.toIr}"

    resultVar
  }
  
  def fptruncTo(resultDest : ResultDestination)(value : IrValue, toType : FloatingPointType) : IrValue = {
    value.irType match {
      case fromType : FloatingPointType =>
        if (fromType.bits <= toType.bits) {
          throw new InconsistentIrException(s"Attempted fptruncto from ${fromType.bits} to ${toType.bits}")
        }

      case _ => throw new InconsistentIrException("Attempted fptruncto from non-floating point type")
    }

    val resultVar = resultDest.asLocalVariable(nameSource, toType)
    instructions += s"${resultVar.toIr} = fptrunc ${value.toIrWithType} to ${toType.toIr}"
    
    resultVar
  }

  def fpextTo(resultDest : ResultDestination)(value : IrValue, toType : FloatingPointType) : IrValue = {
    value.irType match {
      case fromType : FloatingPointType =>
        if (fromType.bits >= toType.bits) {
          throw new InconsistentIrException(s"Attempted fpextto from ${fromType.bits} to ${toType.bits}")
        }

      case _ => throw new InconsistentIrException("Attempted ifpextto from non-floating point type")
    }

    val resultVar = resultDest.asLocalVariable(nameSource, toType)
    instructions += s"${resultVar.toIr} = fpext ${value.toIrWithType} to ${toType.toIr}"

    resultVar
  }
  
  def ptrtoint(resultDest : ResultDestination)(value : IrValue, toType : IntegerType) : IrValue = {
    if (!value.irType.isInstanceOf[PointerType]) {
      throw new InconsistentIrException("Attempted ptrtoint with non-pointer")
    }

    val resultVar = resultDest.asLocalVariable(nameSource, toType)
    instructions += s"${resultVar.toIr} = ptrtoint ${value.toIrWithType} to ${toType.toIr}"

    resultVar
  }

  def inttoptr(resultDest : ResultDestination)(value : IrValue, toType : PointerType) : IrValue = {
    if (!value.irType.isInstanceOf[IntegerType]) {
      throw new InconsistentIrException("Attempted inttoptr with non-integer")
    }

    val resultVar = resultDest.asLocalVariable(nameSource, toType)
    instructions += s"${resultVar.toIr} = inttoptr ${value.toIrWithType} to ${toType.toIr}"

    resultVar
  }
  
  private def genericItofpInstr(instruction : String)(resultDest : ResultDestination)(value : IrValue, toType : FloatingPointType) : IrValue = {
    value.irType match {
      case fromType : IntegerType => // Good to go
      case _ => throw new InconsistentIrException(s"Attempted ${instruction} from non-integer type")
    }
    
    val resultVar = resultDest.asLocalVariable(nameSource, toType)
    instructions += s"${resultVar.toIr} = ${instruction} ${value.toIrWithType} to ${toType.toIr}"

    resultVar
  }

  val uitofp = genericItofpInstr("uitofp")_
  val sitofp = genericItofpInstr("sitofp")_
}
