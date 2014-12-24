package io.llambda.llvmir

// This doesn't extend Irable because the mnemonic needs to be combined with
// an instruction-specific prefix to be valid IR
sealed abstract class IComparisonCond(val mnemonic : String, val signedDependent : Boolean)

object IComparisonCond {
  case object Equal extends IComparisonCond("eq", false)
  case object NotEqual extends IComparisonCond("ne", false)

  case object GreaterThan extends IComparisonCond("gt", true)
  case object GreaterThanEqual extends IComparisonCond("ge", true)

  case object LessThan extends IComparisonCond("lt", true)
  case object LessThanEqual extends IComparisonCond("le", true)
}

sealed abstract class FComparisonCond(mnemonic : String) extends Irable {
  def toIr = mnemonic
}

object FComparisonCond {
  case object False extends FComparisonCond("false")
  case object OrderedEqual extends FComparisonCond("oeq")
  case object OrderedGreaterThan extends FComparisonCond("ogt")
  case object OrderedGreaterThanEqual extends FComparisonCond("oge")
  case object OrderedLessThan extends FComparisonCond("olt")
  case object OrderedLessThanEqual extends FComparisonCond("ole")
  case object OrderedNotEqual extends FComparisonCond("one")
  case object Ordered extends FComparisonCond("ord")
  case object UnorderedEqual extends FComparisonCond("ueq")
  case object UnorderedGreaterThan extends FComparisonCond("ugt")
  case object UnorderedGreaterThanEqual extends FComparisonCond("uge")
  case object UnorderedLessThan extends FComparisonCond("ult")
  case object UnorderedLessThanEqual extends FComparisonCond("ule")
  case object UnorderedNotEqual extends FComparisonCond("une")
  case object Unordered extends FComparisonCond("uno")
  case object True extends FComparisonCond("true")
}

private[llvmir] trait OtherInstrs extends IrInstrBuilder {
  def icmp(resultDest : ResultDestination)(compareCond : IComparisonCond, signed : Option[Boolean], val1 : IrValue, val2 : IrValue) = {
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

  def fcmp(resultDest : ResultDestination)(compareCond : FComparisonCond, val1 : IrValue, val2 : IrValue) = {
    if (val1.irType != val2.irType) {
      throw new InconsistentIrException("Attempted fcmp with incompatible types")
    }

    val comparedType = val1.irType

    if (!comparedType.isInstanceOf[FloatingPointType]) {
      throw new InconsistentIrException("Attempted fcmp with non-floating point type")
    }

    val resultVar = resultDest.asLocalVariable(nameSource, IntegerType(1))
    addInstruction(s"${resultVar.toIr} = fcmp ${compareCond.toIr} ${comparedType.toIr} ${val1.toIr}, ${val2.toIr}")
    resultVar
  }

  def callDecl(resultDestOpt : Option[ResultDestination])(
      decl : IrFunctionDeclLike,
      arguments : Seq[IrValue],
      tailCall : Boolean = false,
      metadata : Map[String, Metadata] = Map()
  ) : Option[LocalVariable] = {
    call(resultDestOpt)(decl, decl.irValue, arguments, tailCall, metadata)
  }

  def call(resultDestOpt : Option[ResultDestination])(
      signature : IrSignatureLike,
      functionPtr : IrValue,
      arguments : Seq[IrValue],
      tailCall : Boolean = false,
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

    addInstruction(callParts.mkString(" "), metadata=metadata)

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
