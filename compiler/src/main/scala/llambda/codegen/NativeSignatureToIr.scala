package llambda.codegen

import llambda.nfi.NativeSignature
import llambda.{boxedtype => bt}
import llambda.codegen.llvmir.{IrSignature, PointerType, VoidType}
import llambda.codegen.llvmir.IrFunction._

object NativeSignatureToIr {
  private def paramSignednessToAttribs(signedness : Option[Boolean]) : Set[ParameterAttribute] = {
    signedness match {
      case Some(true) => Set(SignExt)
      case Some(false) => Set(ZeroExt)
      case None => Set()
    }
  }

  def apply(signature : NativeSignature) : IrSignature = {
    val closureArgs = if (signature.hasClosureArg) {
      List(Argument(PointerType(bt.BoxedProcedure.irType), Set()))
    }
    else {
      Nil
    }

    val fixedArgs = signature.fixedArgs map (NfiTypeToIrType(_)) map {
      case SignedFirstClassType(irType, signedness) =>
        Argument(irType, paramSignednessToAttribs(signedness))
    }

    val restArgs = if (signature.hasRestArg) {
      List(Argument(PointerType(bt.BoxedListElement.irType), Set()))
    }
    else {
      Nil
    } 

    val allArgs = closureArgs ++ fixedArgs ++ restArgs

    val result = signature.returnType map (NfiTypeToIrType(_)) match {
      case None => 
        Result(VoidType, Set())
      case Some(SignedFirstClassType(irType, signedness)) =>
        Result(irType, paramSignednessToAttribs(signedness))
    }

    IrSignature(result=result, arguments=allArgs)
  }
}
