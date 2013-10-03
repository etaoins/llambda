package llambda.codegen

import llambda.et.NativeFunction
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir.{IrCallable, PointerType, VoidType}
import llambda.codegen.llvmir.IrFunction._

object NativeSignatureToIrCallable {
  private def paramSignednessToAttribs(signedness : Option[Boolean]) : Set[ParameterAttribute] = {
    signedness match {
      case Some(true) => Set(SignExt)
      case Some(false) => Set(ZeroExt)
      case None => Set()
    }
  }

  def apply(signature : NativeFunction) : IrCallable = {
    val fixedArgs = signature.fixedArgs map (NfiTypeToIrType(_)) map {
      case SignedFirstClassType(irType, signedness) =>
        Argument(irType, paramSignednessToAttribs(signedness))
    }

    val allArgs : List[Argument] = if (signature.hasRestArg) {
      fixedArgs :+ Argument(PointerType(bt.BoxedPair.irType), Set())
    }
    else {
      fixedArgs
    } 

    val result = signature.returnType map (NfiTypeToIrType(_)) match {
      case None => 
        Result(VoidType, Set())
      case Some(SignedFirstClassType(irType, signedness)) =>
        Result(irType, paramSignednessToAttribs(signedness))
    }

    IrCallable(result=result, arguments=allArgs)
  }
}
