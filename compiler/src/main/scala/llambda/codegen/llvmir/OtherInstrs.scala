package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

protected[llvmir] case class PhiSource(value : IrValue, block : IrBlockBuilder)

private[llvmir] trait OtherInstrs extends IrInstrBuilder {
  def icmp(resultName : String)(compareCond : ComparisonCond.ComparisonCond, signed : Option[Boolean], val1 : IrValue, val2 : IrValue) = {
    if (val1.irType != val2.irType) {
      throw new InternalCompilerErrorException("Attempted icmp with incompatible types")
    }

    val comparedType = val1.irType

    comparedType match {
      case IntegerType(_) =>
      case PointerType(_) =>
      case _ => throw new InternalCompilerErrorException("Attempted icmp with type besides integer or pointer")
    }

    val conditionIr = ((compareCond.signedDependent, signed) match {
      case (true, Some(true)) => "s"
      case (true, Some(false)) => "u"
      case (true, None) => throw new InternalCompilerErrorException("Attempted sign dependent icmp without signed flag") 

      case (false, None) => ""
      case (false, Some(_)) => throw new InternalCompilerErrorException("Attempted sign independent icmp with signed flag") 
    }) + compareCond.mnemonic
    
    val resultVar = allocateLocalVar(IntegerType(1), resultName)

    instructions += s"${resultVar.toIr} = icmp ${conditionIr} ${comparedType.toIr} ${val1.toIr}, ${val2.toIr}"

    resultVar
  }

  def phi(resultName : String)(sources : PhiSource*) : LocalVariable = {
    if (sources.isEmpty) {
      throw new InternalCompilerErrorException("Attempted phi with no sources")
    }

    val resultType = sources.map(_.value.irType) reduceLeft { (currentType, newType) =>
      if (currentType != newType) {
        throw new InternalCompilerErrorException("Attempted phi with incompatible types")
      }

      newType
    }

    val resultVar = allocateLocalVar(resultType, resultName)

    val sourceIr = (sources map { source =>
      s"[ ${source.value.toIr}, %${source.block.label} ]"
    }).mkString(", ")

    instructions += s"${resultVar.toIr} = phi ${resultType.toIr} $sourceIr"

    resultVar
  }

  def callDecl(resultName : Option[String])(decl : IrFunctionDeclLike, arguments : Seq[IrValue], tailCall : Boolean = false) : Option[LocalVariable] = {
    call(resultName)(decl, decl.irValue, arguments, tailCall)
  }

  def call(resultName : Option[String])(callable : IrCallableLike, functionPtr : IrValue, arguments : Seq[IrValue], tailCall : Boolean = false) : Option[LocalVariable] = {
    // We only return a result for non-void result types if they specify a result name
    val resultVarOpt = callable.result.irType match {
      case VoidType =>
        None
      case otherType : FirstClassType =>
        resultName.map(allocateLocalVar(otherType, _))
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

    // Add our calling convention if we're using a non-default one
    val callingConvIrOpt = callable.callingConv.toOptIr

    // Only zeroext, signext, inreg are allowed here
    // We don't support inreg
    val filteredRetAttrs = callable.result.attributes.intersect(Set(IrFunction.ZeroExt, IrFunction.SignExt))
    val retAttrIrs = filteredRetAttrs.map(_.toIr)

    val resultTypeIr = callable.result.irType.toIr

    val functionPtrIr = functionPtr.toIr

    if (arguments.length != callable.arguments.length) {
      throw new InternalCompilerErrorException("Passed argument count didn't match callable argument count")
    }

    // Make sure the argument types match the ones expected by the callable
    val argIr = (arguments zip (callable.arguments)) map { case (arg, argDecl) =>
      if (arg.irType != argDecl.irType) {
        throw new InternalCompilerErrorException(s"Argument passed with ${arg.irType}, callable as ${argDecl.irType}")
      }

      arg.toIrWithType
    } mkString(", ")

    // Only noreturn, nounwind, readnone, and readonly are allowed here
    val filteredFuncAttrs = callable.attributes.intersect(Set(IrFunction.NoReturn, IrFunction.NoUnwind, IrFunction.ReadNone, IrFunction.ReadOnly))
    val funcAttrIrs = filteredFuncAttrs.map(_.toIr).toList.sorted

    // Start string building
    val callKernel = functionPtr.toIr + "(" + argIr + ")"

    val callParts : List[String] =
      assignmentIrOpt.toList ++ tailCallIrOpt.toList ++ List("call") ++ callingConvIrOpt.toList ++ retAttrIrs ++ List(resultTypeIr) ++ List(callKernel) ++ funcAttrIrs

    instructions += callParts.mkString(" ")

    resultVarOpt
  }
}
