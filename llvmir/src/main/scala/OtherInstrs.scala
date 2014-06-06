package io.llambda.llvmir

object ComparisonCond {
  // This doesn't extend Irable because the mnemonic needs to be combined with
  // an instruction-specific prefix to be valid IR
  sealed abstract class ComparisonCond(val mnemonic : String, val signedDependent : Boolean)

  case object Equal extends ComparisonCond("eq", false)
  case object NotEqual extends ComparisonCond("ne", false)
  
  case object GreaterThan extends ComparisonCond("gt", true)
  case object GreaterThanEqual extends ComparisonCond("ge", true)
  
  case object LessThan extends ComparisonCond("lt", true)
  case object LessThanEqual extends ComparisonCond("le", true)
}

private[llvmir] trait OtherInstrs extends IrInstrBuilder {
  def icmp(resultDest : ResultDestination)(compareCond : ComparisonCond.ComparisonCond, signed : Option[Boolean], val1 : IrValue, val2 : IrValue) = {
    if (val1.irType != val2.irType) {
      throw new InconsistentIrException("Attempted icmp with incompatible types")
    }

    val comparedType = val1.irType

    comparedType match {
      case IntegerType(_) =>
      case PointerType(_) =>
      case _ => throw new InconsistentIrException("Attempted icmp with type besides integer or pointer")
    }

    val conditionIr = ((compareCond.signedDependent, signed) match {
      case (true, Some(true)) => "s"
      case (true, Some(false)) => "u"
      case (true, None) => throw new InconsistentIrException("Attempted sign dependent icmp without signed flag") 

      case (false, None) => ""
      case (false, Some(_)) => throw new InconsistentIrException("Attempted sign independent icmp with signed flag") 
    }) + compareCond.mnemonic
    
    val resultVar = resultDest.asLocalVariable(nameSource, IntegerType(1))

    addInstruction(s"${resultVar.toIr} = icmp ${conditionIr} ${comparedType.toIr} ${val1.toIr}, ${val2.toIr}")

    resultVar
  }

  def callDecl(resultDestOpt : Option[ResultDestination])(decl : IrFunctionDeclLike, arguments : Seq[IrValue], tailCall : Boolean = false) : Option[LocalVariable] = {
    call(resultDestOpt)(decl, decl.irValue, arguments, tailCall)
  }

  def call(resultDestOpt : Option[ResultDestination])(signature : IrSignatureLike, functionPtr : IrValue, arguments : Seq[IrValue], tailCall : Boolean = false) : Option[LocalVariable] = {
    // We only return a result for non-void result types if they specify a result name
    val resultVarOpt = signature.result.irType match {
      case VoidType =>
        None
      case otherType : FirstClassType =>
        resultDestOpt.map(_.asLocalVariable(nameSource, otherType))
    }

    // If we're non-void we return a value
    val assignmentIrOpt = resultVarOpt.map(_.toIr + " =")

    // Tail calls are prefixed with "tail"
    val tailCallIrOpt = if (tailCall) {
      Some("tail")
    }
    else {
      None
    }

    // Start string building
    val callBody = CallLikeInstructionBody(signature, functionPtr, arguments)
    val callParts = assignmentIrOpt.toList ++ tailCallIrOpt.toList ++ List("call", callBody)

    addInstruction(callParts.mkString(" "))

    resultVarOpt
  }

  def select(resultDest : ResultDestination)(cond : IrValue, trueValue : IrValue, falseValue : IrValue) = {
    if (cond.irType != IntegerType(1)) {
      throw new InconsistentIrException("Attempted to select using non-i1")
    }
    
    if (trueValue.irType != falseValue.irType) {
      throw new InconsistentIrException("Attempted select with incompatible types")
    }
    
    val resultType = trueValue.irType
    val resultVar = resultDest.asLocalVariable(nameSource, resultType)

    addInstruction(s"${resultVar.toIr} = select ${cond.toIrWithType}, ${trueValue.toIrWithType}, ${falseValue.toIrWithType}")

    resultVar
  }
  
  def landingpad(resultDest : ResultDestination)(resultType : FirstClassType,  personalityFunction : IrValue, clauses : Seq[LandingpadClause], cleanup : Boolean = false) : LocalVariable = {
    if (clauses.isEmpty && !cleanup) {
      throw new InconsistentIrException("Attempted non-cleanup landingpad with no clauses")
    }

    val cleanupClauseOpt = if (cleanup) {
      Some("cleanup")
    }
    else {
      None
    }

    val clausesIr = cleanupClauseOpt.toList ++ clauses.map(_.toIr)

    val resultVar = resultDest.asLocalVariable(nameSource, resultType)
    addInstruction(s"${resultVar.toIr} = landingpad ${resultType.toIr} personality ${personalityFunction.toIrWithType} " + clausesIr.mkString(" "))
    resultVar
  }
}
