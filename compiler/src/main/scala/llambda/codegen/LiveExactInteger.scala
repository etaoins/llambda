package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveExactInteger(constantValue : Int) extends ConstantLiveValue(bt.BoxedExactInteger) {
  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    val boxedIntName = module.nameSource.allocate("schemeExactInteger")

    val boxedInt = bt.BoxedExactInteger.createConstant(
      value=IntegerConstant(IntegerType(64), constantValue)
    )

    declareBoxedConstant(module)(boxedIntName, boxedInt)
  }
  
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = {
    case intType : nfi.IntType =>
      IntegerConstant(IntegerType(intType.bits), constantValue)
    case nfi.Float =>
      SingleConstant(constantValue.toFloat)
    case nfi.Double =>
      DoubleConstant(constantValue.toDouble)
  }
}

object LiveExactInteger {
  def fromConstant(value : Int) : ConstantLiveValue =
    new ConstantLiveExactInteger(value)
} 
