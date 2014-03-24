package io.llambda.llvmir

private[llvmir] object CallLikeInstructionBody {
  def apply(signature : IrSignatureLike, functionPtr : IrValue, arguments : Seq[IrValue]) : String = {
    // Add our calling convention if we're using a non-default one
    val callingConvIrOpt = signature.callingConv.toOptIr

    // Only zeroext, signext, inreg are allowed here
    // We don't support inreg
    val filteredRetAttrs = signature.result.attributes.intersect(Set(IrFunction.ZeroExt, IrFunction.SignExt))
    val retAttrIrs = filteredRetAttrs.map(_.toIr)

    val resultTypeIr = signature.result.irType.toIr

    val functionPtrIr = functionPtr.toIr

    if (arguments.length != signature.arguments.length) {
      throw new InconsistentIrException("Passed argument count didn't match signature argument count")
    }

    // Make sure the argument types match the ones expected by the signature
    val argIr = (arguments zip (signature.arguments)) map { case (arg, argDecl) =>
      if (arg.irType != argDecl.irType) {
        throw new InconsistentIrException(s"Argument passed with ${arg.irType}, signature as ${argDecl.irType}")
      }

      arg.toIrWithType
    } mkString(", ")

    // Only noreturn, nounwind, readnone, and readonly are allowed here
    val filteredFuncAttrs = signature.attributes.intersect(Set(IrFunction.NoReturn, IrFunction.NoUnwind, IrFunction.ReadNone, IrFunction.ReadOnly))
    val funcAttrIrs = filteredFuncAttrs.map(_.toIr).toList.sorted

    // Start string building
    val callKernel = functionPtr.toIr + "(" + argIr + ")"

    val callBodyParts = callingConvIrOpt.toList ++ retAttrIrs ++ List(resultTypeIr) ++ List(callKernel) ++ funcAttrIrs

    callBodyParts.mkString(" ")
  }
}

