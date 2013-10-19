package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveString(module : IrModuleBuilder)(constantValue : String) extends ConstantLiveStringLike(module)(constantValue, bt.BoxedString) {
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = {
    case nfi.Utf8CString =>
      genUtf8Constant().irValue
  }
}

private class UnboxedLiveUtf8String(unboxedValue : IrValue) extends UnboxedLiveValue(bt.BoxedExactInteger, nfi.Utf8CString, unboxedValue) {
  def genBoxedValue(state : GenerationState) : (GenerationState, IrValue) = {
    val block = state.currentBlock

    // Make sure _lliby_string_from_utf8 is declared
    val llibyStringFromUtf8Decl = IrFunctionDecl(
      result=IrFunction.Result(PointerType(bt.BoxedString.irType)),
      name="_lliby_string_from_utf8",
      arguments=List(IrFunction.Argument(PointerType(IntegerType(8)))),
      attributes=Set(IrFunction.NoUnwind)
    )

    state.module.unlessDeclared(llibyStringFromUtf8Decl) {
      state.module.declareFunction(llibyStringFromUtf8Decl)
    }

    val boxedValue = block.callDecl(Some("boxedString"))(llibyStringFromUtf8Decl, List(unboxedValue)).get
    (state, boxedValue)
  }
}

object LiveString {
  def fromConstant(module : IrModuleBuilder)(value : String) : ConstantLiveValue =
    new ConstantLiveString(module)(value)

  def fromUtf8Unboxed(unboxedValue : IrValue) : LiveValue =
    new UnboxedLiveUtf8String(unboxedValue)
  
  def genUtf8Unboxing(block : IrBlockBuilder)(boxedValue : IrValue) : IrValue = {
    val pointerToValue = bt.BoxedString.genPointerToUtf8Data(block)(boxedValue)
    block.load("unboxedUtf8Data")(pointerToValue)
  }
} 
