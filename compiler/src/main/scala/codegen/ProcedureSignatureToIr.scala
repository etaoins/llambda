package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{celltype => ct}
import llambda.compiler.codegen.llvmir.{IrSignature, PointerType, VoidType, IntegerType}
import llambda.compiler.codegen.llvmir.IrFunction._

object ProcedureSignatureToIr {
  private def paramSignednessToAttribs(signedness : Option[Boolean]) : Set[ParameterAttribute] = {
    signedness match {
      case Some(true) => Set(SignExt)
      case Some(false) => Set(ZeroExt)
      case None => Set()
    }
  }

  def apply(signature : ProcedureSignature) : IrSignature = {
    val selfArgs = if (signature.hasSelfArg) {
      List(Argument(PointerType(ct.ProcedureCell.irType), Set()))
    }
    else {
      Nil
    }

    val fixedArgs = signature.fixedArgs map (ValueTypeToIr(_)) map {
      case SignedFirstClassType(irType, signedness) =>
        Argument(irType, paramSignednessToAttribs(signedness))
    }

    val restArgs = if (signature.hasRestArg) {
      List(Argument(PointerType(ct.ListElementCell.irType), Set()))
    }
    else {
      Nil
    } 

    val allArgs = selfArgs ++ fixedArgs ++ restArgs

    val result = signature.returnType map (ValueTypeToIr(_)) match {
      case None => 
        Result(VoidType, Set())
      case Some(SignedFirstClassType(irType, signedness)) =>
        Result(irType, paramSignednessToAttribs(signedness))
    }

    IrSignature(result=result, arguments=allArgs)
  }
}
