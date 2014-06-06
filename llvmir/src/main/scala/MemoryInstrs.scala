package io.llambda.llvmir

private[llvmir] trait MemoryInstrs extends IrInstrBuilder {
  def alloca(resultDest : ResultDestination)(irType : IrType, numElements : Int = 1, alignment : Int = 0) : LocalVariable = {
    val resultVar = resultDest.asLocalVariable(nameSource, PointerType(irType))

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

    addInstruction((List(baseAlloc) ++ numElementsOptIr.toList ++ alignOptIr.toList).mkString(", "))

    resultVar
  }
  
  private def pointeeTypeForAccess(irType : IrType) : FirstClassType = irType match {
    case pointerType : PointerType =>
      pointerType.pointeeType match {
        case firstClass : FirstClassType =>
          firstClass
        case _ =>
          throw new InconsistentIrException("Attempted memory access with a pointer to a non-first class type")
      }
    case _ =>
      throw new InconsistentIrException("Attempted memory access from a non-pointer")
  }

  def load(resultDest : ResultDestination)(from : IrValue, alignment : Int = 0, volatile : Boolean = false, metadata : Map[String, Metadata] = Map()) : LocalVariable = {
    val resultType = pointeeTypeForAccess(from.irType)
    val resultVar = resultDest.asLocalVariable(nameSource, resultType)

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
    
    addInstruction(s"${resultVar.toIr} = load${volatileIr} ${from.toIrWithType}${alignIr}", metadata)

    resultVar
  }

  def store(value : IrValue, to : IrValue, alignment : Int = 0, volatile : Boolean = false, metadata : Map[String, Metadata] = Map()) : Unit = {
    val storedType = pointeeTypeForAccess(to.irType)

    if (storedType != value.irType) {
      throw new InconsistentIrException(s"Attempted to store value to pointer of incompatible type\n" +
                                        s"Value type: ${value.irType}\n" +
                                        s"Pointer type: ${storedType}")
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
    
    addInstruction(s"store${volatileIr} ${value.toIrWithType}, ${to.toIrWithType}${alignIr}", metadata)
  }

  def getelementptr(resultDest : ResultDestination)(elementType : FirstClassType, basePointer : IrValue, indices : Seq[IrValue], inbounds : Boolean = false) : LocalVariable = {
    val resultVar = resultDest.asLocalVariable(nameSource, PointerType(elementType))

    basePointer.irType match {
      case pointerType : PointerType =>
        if (indices.length == 1) {
          val pointeeType = pointerType.pointeeType

          if (elementType != pointerType.pointeeType) {
            throw new InconsistentIrException(s"getelementptr passed element type ${elementType}; actual element type is ${pointeeType}")
          }
        }
      case _ =>
        throw new InconsistentIrException("Attempted getelementptr from non-pointer base pointer")
    }

    indices map(_.irType) foreach { 
      case IntegerType(_) =>
      case _ =>
        throw new InconsistentIrException("Attempted getelementptr with non-integer index")
    }

    val inboundsIr = if (inbounds) {
      " inbounds"
    }
    else {
      ""
    }

    val baseIr = s"${resultVar.toIr} = getelementptr${inboundsIr} ${basePointer.toIrWithType}"

    val irParts = baseIr :: indices.toList.map(_.toIrWithType)
    addInstruction(irParts.mkString(", "))

    resultVar
  }
}
