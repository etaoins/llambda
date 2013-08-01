package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

private[llvmir] trait OtherInstrs extends IrBuilder {
  protected case class PhiSource(value : IrValue, label : IrLabel)

  protected def phi(sources : PhiSource*) : LocalVariable = {
    if (sources.isEmpty) {
      throw new InternalCompilerErrorException("Attempted phi with no sources")
    }

    val resultType = sources.map(_.value.irType) reduceLeft { (currentType, newType) =>
      if (currentType != newType) {
        throw new InternalCompilerErrorException("Attempted phi with incompatible types")
      }

      newType
    }

    val resultVar = allocateLocalVar(resultType)

    val sourceIr = (sources map { source =>
      s"[ ${source.value.toIr}, ${source.label.toIr} ]"
    }).mkString(", ")

    instructions += s"${resultVar.toIr} = phi ${resultType.toIr} $sourceIr"

    resultVar
  }

  protected def callDecl(decl : IrFunctionDeclLike, arguments : Seq[IrValue], tailCall : Boolean = false) : Option[LocalVariable] = {
    call(decl, decl.irValue, arguments, tailCall)
  }

  protected def call(callable : IrCallableLike, functionPtr : IrValue, arguments : Seq[IrValue], tailCall : Boolean = false) : Option[LocalVariable] = {
    // We only return a result for non-void result types
    val resultVarOpt = callable.result.irType match {
      case VoidType =>
        None
      case otherType : FirstClassType =>
        Some(allocateLocalVar(otherType))
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

    // Combine the passed arguments with their attributes from their callable
    val argIr = (arguments zip (callable.arguments)) map { case (arg, argDecl) =>
      if (arg.irType != argDecl.irType) {
        throw new InternalCompilerErrorException(s"Argument passed with ${arg.irType}, callable as ${argDecl.irType}")
      }

      (arg.toIrWithType :: argDecl.attributes.map(_.toIr).toList.sorted).mkString(" ")
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