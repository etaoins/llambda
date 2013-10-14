package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveCharacter(codePoint : Char) extends ConstantLiveValue(bt.BoxedCharacter) {
  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    val boxedCharName = module.nameSource.allocate("schemeCharacter")

    val boxedChar = bt.BoxedCharacter.createConstant(
      unicodeChar=IntegerConstant(IntegerType(32), codePoint)
    )

    declareBoxedConstant(module)(boxedCharName, boxedChar)
  }
  
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = {
    case nfi.UnicodeChar =>
      IntegerConstant(IntegerType(32), codePoint)
  }
}

private class UnboxedLiveCharacter(unboxedValue : IrValue) extends UnboxedLiveValue(bt.BoxedCharacter, nfi.UnicodeChar, unboxedValue) {
  def genBoxedValue(state : GenerationState) : IrValue = {
    val block = state.currentBlock

    // Make sure _lliby_box_character is declared
    val llibyBoxCharacterDecl = IrFunctionDecl(
      result=IrFunction.Result(PointerType(bt.BoxedCharacter.irType)),
      name="_lliby_box_character",
      arguments=List(IrFunction.Argument(IntegerType(32))),
      attributes=Set(IrFunction.NoUnwind)
    )

    state.module.unlessDeclared(llibyBoxCharacterDecl) {
      state.module.declareFunction(llibyBoxCharacterDecl)
    }

    block.callDecl(Some("boxedChar"))(llibyBoxCharacterDecl, List(unboxedValue)).get
  }
}

object LiveCharacter {
  def fromConstant(value : Char) : ConstantLiveValue =
    new ConstantLiveCharacter(value)

  def fromUnboxed(unboxedValue : IrValue) : LiveValue =
    new UnboxedLiveCharacter(unboxedValue)
} 

