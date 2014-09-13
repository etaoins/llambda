package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{ProcedureSignature, ProcedureAttribute}
import llambda.compiler.{celltype => ct}
import llambda.llvmir.{IrSignature, PointerType, VoidType, IntegerType, CallingConv}
import llambda.llvmir.IrFunction._
import llambda.compiler.{valuetype => vt}

object ProcedureSignatureToIr {
  private def paramSignednessToAttribs(signedness : Option[Boolean]) : Set[ParameterAttribute] = {
    signedness match {
      case Some(true) => Set(SignExt)
      case Some(false) => Set(ZeroExt)
      case None => Set()
    }
  }

  def apply(signature : ProcedureSignature) : IrSignature = {
    val worldArgs = if (signature.hasWorldArg) {
      List(Argument(PointerType(WorldValue.irType), Set()))
    }
    else {
      Nil
    }

    val selfArgs = if (signature.hasSelfArg) {
      List(Argument(PointerType(ct.ProcedureCell.irType), Set()))
    }
    else {
      Nil
    }

    val fixedArgs = signature.fixedArgTypes map (ValueTypeToIr(_)) map {
      case SignedFirstClassType(irType, signedness) =>
        Argument(irType, paramSignednessToAttribs(signedness))
    }

    val restArgs = if (signature.restArgMemberTypeOpt.isDefined) {
      List(Argument(PointerType(ct.ListElementCell.irType), Set()))
    }
    else {
      Nil
    } 

    val allArgs = worldArgs ++ selfArgs ++ fixedArgs ++ restArgs

    val result = signature.returnType.representationTypeOpt match {
      case None =>
        Result(VoidType, Set())

      case Some(valueType) =>
        val signedType = ValueTypeToIr(valueType)
        Result(signedType.irType, paramSignednessToAttribs(signedType.signed))
    }

    // Convert our internal attributes to LLVM IR ones if applicable
    val explicitAttributes = signature.attributes.collect {
      case ProcedureAttribute.NoReturn =>
        NoReturn
    } : Set[FunctionAttribute]

    val attributes = if (signature.hasWorldArg) {
      // World functions can throw exceptions
      explicitAttributes
    }
    else {
      // Non-world ative functions must not throw exceptions
      // We don't generate a GC safe point for them
      explicitAttributes + NoUnwind
    }

    val callingConv = if (signature.attributes.contains(ProcedureAttribute.FastCC)) {
      CallingConv.FastCC
    }
    else {
      CallingConv.Default
    }

    IrSignature(result=result, arguments=allArgs, attributes=attributes, callingConv=callingConv)
  }
}
