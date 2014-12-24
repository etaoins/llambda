package io.llambda.llvmir
 
private[llvmir] trait TerminatorInstrs extends IrInstrBuilder {
  def ret(value : IrValue) {
    addInstruction(s"ret ${value.toIrWithType}")
    terminateBlock()
  }

  def retVoid() {
    addInstruction("ret void")
    terminateBlock()
  }

  def condBranch(cond : IrValue, trueBlock : IrBranchTarget, falseBlock : IrBranchTarget) {
    if (cond.irType != IntegerType(1)) {
      throw new InconsistentIrException("Attempted to branch using non-i1")
    }

    addInstruction(s"br ${cond.toIrWithType}, label %${trueBlock.label}, label %${falseBlock.label}")
    terminateBlock()
  }

  def uncondBranch(block : IrBranchTarget) {
    addInstruction(s"br label %${block.label}")
    terminateBlock()
  }

  def unreachable() {
    addInstruction("unreachable")
    terminateBlock()
  }

  def switch(testValue : IrValue, defaultBlock : IrBranchTarget, entries : (Long, IrBranchTarget)*) {
    val testValueType = testValue.irType match {
      case integerType : IntegerType =>
        integerType

      case _ =>
        throw new InconsistentIrException("Attempted switch with non-integer type")
    }

    entries.foldLeft(Set[Long]()) { case (seenValues, (compareConstant, _)) =>
      if (seenValues.contains(compareConstant)) {
        throw new InconsistentIrException(s"Attempted switch with duplicate comparison constant of ${compareConstant}")
      }

      seenValues + compareConstant
    }

    val entriesIr = (entries map { case (value, targetBlock) =>
      // Build an IR constant of the correct type:
      val irConstant = IntegerConstant(testValueType, value)

      s"${irConstant.toIrWithType}, label %${targetBlock.label}"
    }).mkString("  ")

    addInstruction(s"switch ${testValue.toIrWithType}, label %${defaultBlock.label} [ ${entriesIr} ]")
    terminateBlock()
  }
  
  def invokeDecl(resultDestOpt : Option[ResultDestination])(
      decl : IrFunctionDeclLike,
      arguments : Seq[IrValue],
      normalBlock : IrBranchTarget,
      exceptionBlock : IrBranchTarget,
      metadata : Map[String, Metadata] = Map()
  ) : Option[LocalVariable] = {
    invoke(resultDestOpt)(decl, decl.irValue, arguments, normalBlock, exceptionBlock, metadata)
  }

  def invoke(resultDestOpt : Option[ResultDestination])(
      signature : IrSignatureLike,
      functionPtr : IrValue,
      arguments : Seq[IrValue],
      normalBlock : IrBranchTarget,
      exceptionBlock : IrBranchTarget,
      metadata : Map[String, Metadata] = Map()
  ) : Option[LocalVariable] = {
    // We only return a result for non-void result types if they specify a result name
    val resultVarOpt = signature.result.irType match {
      case VoidType =>
        None
      case otherType : FirstClassType =>
        resultDestOpt.map(_.asLocalVariable(nameSource, otherType))
    }

    // If we're non-void we return a value
    val assignmentIrOpt = resultVarOpt.map(_.toIr + " =")

    // Build our target blocks
    val targetBlocksIr = List(
      s"to label %${normalBlock.label}",
      s"unwind label %${exceptionBlock.label}"
    )

    // Start string building
    val callBody = CallLikeInstructionBody(signature, functionPtr, arguments)
    val callParts = assignmentIrOpt.toList ++ List("invoke") ++ List(callBody) ++ targetBlocksIr

    addInstruction(callParts.mkString(" "), metadata=metadata)
    terminateBlock()

    resultVarOpt
  }

  def resume(resumeValue : IrValue) {
    addInstruction(s"resume ${resumeValue.toIrWithType}")
    terminateBlock()
  }
}

