package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveCharacter(module : IrModuleBuilder)(codePoint : Char) extends ConstantLiveValue(bt.BoxedCharacter) {
  def genBoxedConstant() : IrConstant = {
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
  def genBoxedValue(initialState : GenerationState) : (GenerationState, IrValue) = {
    // Allocate the cons
    val (state, allocation) = GenConsAllocation(initialState)(1)

    val block = state.currentBlock

    // Initialize it
    val boxedCharCons = allocation.genTypedPointer(state)(0, bt.BoxedCharacter) 
    bt.BoxedCharacter.genStoreToUnicodeChar(block)(unboxedValue, boxedCharCons)

    (state, boxedCharCons)
  }
}

object LiveCharacter {
  def fromConstant(module : IrModuleBuilder)(value : Char) : ConstantLiveValue =
    new ConstantLiveCharacter(module)(value)

  def fromUnboxed(unboxedValue : IrValue) : LiveValue =
    new UnboxedLiveCharacter(unboxedValue)
  
  def genUnboxing(block : IrBlockBuilder)(boxedValue : IrValue) : IrValue = {
    val pointerToUnicodeChar = bt.BoxedCharacter.genPointerToUnicodeChar(block)(boxedValue)
    block.load("unboxedUnicodeChar")(pointerToUnicodeChar)
  }
} 

