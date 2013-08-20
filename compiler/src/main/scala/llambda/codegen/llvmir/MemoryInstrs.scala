package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

private[llvmir] trait MemoryInstrs extends IrInstrBuilder {
  def alloca(resultName : String)(irType : IrType, numElements : Integer = 1, alignment : Integer = 0) : LocalVariable = {
    val resultVar = allocateLocalVar(PointerType(irType), resultName)

    val baseAlloc = s"${resultVar.toIr} = alloca ${irType.toIr}"

    val numElementsOptIr = if (numElements == 1) {
      None
    }
    else {
      Some(s"i32 ${numElements}")
    }

    val alignOptIr = if (alignment == 0) {
      None
    }
    else {
      Some(s"align ${alignment}")
    }

    instructions += (List(baseAlloc) ++ numElementsOptIr.toList ++ alignOptIr.toList).mkString(", ")

    resultVar
  }
  
  private def pointeeTypeForAccess(irType : IrType) : FirstClassType = irType match {
    case pointerType : PointerType =>
      pointerType.pointeeType match {
        case firstClass : FirstClassType =>
          firstClass
        case _ =>
          throw new InternalCompilerErrorException("Attempted memory access with a pointer to a non-first class type")
      }
    case _ =>
      throw new InternalCompilerErrorException("Attempted memory access from a non-pointer")
  }

  def load(resultName : String)(from : IrValue, alignment : Integer = 0, volatile : Boolean = false) : LocalVariable = {
    val resultType = pointeeTypeForAccess(from.irType)
    val resultVar = allocateLocalVar(resultType, resultName)

    val volatileIr = if (volatile) {
      " volatile"
    }
    else {
      ""
    }

    val alignIr = if (alignment != 0) {
      s", align ${alignment}"
    }
    else {
      ""
    }

    instructions += s"${resultVar.toIr} = load${volatileIr} ${from.toIrWithType}${alignIr}"

    resultVar
  }

  def store(value : IrValue, to : IrValue, alignment : Integer = 0, volatile : Boolean = false) : Unit = {
    val storedType = pointeeTypeForAccess(to.irType)

    if (storedType != value.irType) {
      throw new InternalCompilerErrorException("Attempted to store value to pointer of incompatible type")
    }
    
    val volatileIr = if (volatile) {
      " volatile"
    }
    else {
      ""
    }

    val alignIr = if (alignment != 0) {
      s", align ${alignment}"
    }
    else {
      ""
    }

    instructions += s"store${volatileIr} ${value.toIrWithType}, ${to.toIrWithType}${alignIr}"
  }

  def getelementptr(resultName : String)(resultType : FirstClassType, basePointer : IrValue, indices : Seq[Integer], inbounds : Boolean = false) : LocalVariable = {
    val resultVar = allocateLocalVar(resultType, resultName)

    basePointer.irType match {
      case PointerType(_) =>
        // We're cool
      case _ =>
        throw new InternalCompilerErrorException("Attempted getelementptr from non-pointer base pointer")
    }

    val inboundsIr = if (inbounds) {
      " inbounds"
    }
    else {
      ""
    }

    val baseIr = s"${resultVar.toIr} = getelementptr${inboundsIr} ${basePointer.toIrWithType}"

    val irParts = baseIr :: indices.toList.map("i32 " + _.toString)
    instructions += irParts.mkString(", ")

    resultVar
  }
}
