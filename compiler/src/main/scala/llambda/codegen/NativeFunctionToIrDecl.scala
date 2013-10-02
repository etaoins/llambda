package llambda.codegen

import llambda.et.NativeFunction
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir.{IrFunctionDecl, PointerType, VoidType}
import llambda.codegen.llvmir.IrFunction._

object NativeFunctionToIrDecl {
  private def paramSignednessToAttribs(signedness : Option[Boolean]) : Set[ParameterAttribute] = {
    signedness match {
      case Some(true) => Set(SignExt)
      case Some(false) => Set(ZeroExt)
      case None => Set()
    }
  }

  def apply(nativeFunc : NativeFunction) : IrFunctionDecl = {
    val fixedArgs = nativeFunc.fixedArgs map (NfiTypeToIrType(_)) map {
      case SignedFirstClassType(irType, signedness) =>
        Argument(irType, paramSignednessToAttribs(signedness))
    }

    val allArgs : List[Argument] = if (nativeFunc.hasRestArg) {
      fixedArgs :+ Argument(PointerType(bt.BoxedPair.irType), Set())
    }
    else {
      fixedArgs
    } 

    val result = nativeFunc.returnType map (NfiTypeToIrType(_)) match {
      case None => 
        Result(VoidType, Set())
      case Some(SignedFirstClassType(irType, signedness)) =>
        Result(irType, paramSignednessToAttribs(signedness))
    }

    IrFunctionDecl(result=result, name=nativeFunc.nativeSymbol, arguments=allArgs)
  }
}
