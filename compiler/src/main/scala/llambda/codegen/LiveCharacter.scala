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

object LiveCharacter {
  def fromConstant(value : Char) : ConstantLiveValue =
    new ConstantLiveCharacter(value)
} 

