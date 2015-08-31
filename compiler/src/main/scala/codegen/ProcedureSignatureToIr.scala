package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{ProcedureSignature, ProcedureAttribute}
import llambda.compiler.{celltype => ct}
import llambda.llvmir._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ast

object ProcedureSignatureToIr {
  case class Result(
      irSignature : IrSignature,
      callMetadata : Map[String, Metadata]
  )

  private def paramSignednessToAttribs(signedness : Option[Boolean]) : Set[IrFunction.ParameterAttribute] = {
    signedness match {
      case Some(true) => Set(IrFunction.SignExt)
      case Some(false) => Set(IrFunction.ZeroExt)
      case None => Set()
    }
  }

  def apply(signature : ProcedureSignature) : Result = {
    val worldArgs = if (signature.hasWorldArg) {
      List(IrFunction.Argument(PointerType(WorldValue.irType), Set()))
    }
    else {
      Nil
    }

    val selfArgs = if (signature.hasSelfArg) {
      List(IrFunction.Argument(PointerType(ct.ProcedureCell.irType), Set()))
    }
    else {
      Nil
    }

    val mandatoryArgs = signature.mandatoryArgTypes map (ValueTypeToIr(_)) map {
      case SignedFirstClassType(irType, signedness) =>
        IrFunction.Argument(irType, paramSignednessToAttribs(signedness))
    }

    val varArgs = if ((signature.optionalArgTypes.length > 0) || signature.restArgMemberTypeOpt.isDefined) {
      List(IrFunction.Argument(PointerType(ct.ListElementCell.irType), Set()))
    }
    else {
      Nil
    }

    val allArgs = worldArgs ++ selfArgs ++ mandatoryArgs ++ varArgs

    val result = signature.returnType.representationTypeOpt match {
      case None =>
        IrFunction.Result(VoidType, Set())

      case Some(valueType) =>
        val signedType = ValueTypeToIr(valueType)
        IrFunction.Result(signedType.irType, paramSignednessToAttribs(signedType.signed))
    }

    val callMetadata = (signature.returnType match {
      case vt.ReturnType.SingleValue(vt.UnicodeChar) =>
        val int32Type = IntegerType(32)

        Map(
          "range" -> RangeMetadata(int32Type, (ast.CharLiteral.firstCodePoint, ast.CharLiteral.lastCodePoint + 1))
        )

      case _ =>
        Map()
    }) : Map[String, Metadata]

    // Convert our internal attributes to LLVM IR ones if applicable
    val explicitAttributes = signature.attributes.collect {
      case ProcedureAttribute.NoReturn =>
        IrFunction.NoReturn
    } : Set[IrFunction.FunctionAttribute]

    val attributes = if (signature.hasWorldArg) {
      // World functions can throw exceptions
      explicitAttributes
    }
    else {
      // Non-world ative functions must not throw exceptions
      // We don't generate a GC safe point for them
      explicitAttributes + IrFunction.NoUnwind
    }

    val callingConv = if (signature.attributes.contains(ProcedureAttribute.FastCC)) {
      CallingConv.FastCC
    }
    else {
      CallingConv.Default
    }

    Result(
      irSignature=IrSignature(result=result, arguments=allArgs, attributes=attributes, callingConv=callingConv),
      callMetadata=callMetadata
    )
  }
}
