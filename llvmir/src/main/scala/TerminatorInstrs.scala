package io.llambda.llvmir
 
private[llvmir] trait TerminatorInstrs extends IrInstrBuilder {
  def ret(value : IrValue) {
    instructions += s"ret ${value.toIrWithType}"
  }

  def retVoid() {
    instructions += "ret void"
  }

  def condBranch(cond : IrValue, trueBlock : IrBranchTarget, falseBlock : IrBranchTarget) {
    if (cond.irType != IntegerType(1)) {
      throw new InconsistentIrException("Attempted to branch using non-i1")
    }

    instructions += s"br ${cond.toIrWithType}, label %${trueBlock.label}, label %${falseBlock.label}"
  }

  def uncondBranch(block : IrBranchTarget) {
    instructions += s"br label %${block.label}"
  }

  def unreachable() {
    instructions += "unreachable"
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

    instructions += s"switch ${testValue.toIrWithType}, label %${defaultBlock.label} [ ${entriesIr} ]"
  }
  
  def invokeDecl(resultDestOpt : Option[ResultDestination])(decl : IrFunctionDeclLike, arguments : Seq[IrValue], normalBlock : IrBranchTarget, exceptionBlock : IrBranchTarget) : Option[LocalVariable] = {
    invoke(resultDestOpt)(decl, decl.irValue, arguments, normalBlock, exceptionBlock)
  }
  
  def invoke(resultDestOpt : Option[ResultDestination])(signature : IrSignatureLike, functionPtr : IrValue, arguments : Seq[IrValue], normalBlock : IrBranchTarget, exceptionBlock : IrBranchTarget) : Option[LocalVariable] = {
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

    instructions += callParts.mkString(" ")

    resultVarOpt
  }

  def resume(resumeValue : IrValue) {
    instructions += s"resume ${resumeValue.toIrWithType}"
  }
}

