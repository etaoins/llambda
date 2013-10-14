package llambda.codegen

import llambda.et
import llambda.codegen.llvmir._

object GenNativeFunction {
  def apply(module : IrModuleBuilder)(nativeFunc : et.NativeFunction) : LiveValue = {
    // Declare the function
    val irSignature = NativeSignatureToIr(nativeFunc)

    val irFunctionDecl = IrFunctionDecl(
      result=irSignature.result,
      name=nativeFunc.nativeSymbol,
      arguments=irSignature.arguments,
      attributes=irSignature.attributes,
      callingConv=irSignature.callingConv)

    module.unlessDeclared(irFunctionDecl) {
      module.declareFunction(irFunctionDecl)
    }

    // Wrap it in a LiveValue
    // We'll lazily generated the boxed version later if needed
    new LiveProcedure(
      signature=nativeFunc,
      functionPointer=irFunctionDecl.irValue) 
  }
}
