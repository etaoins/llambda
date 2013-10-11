package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveInexactRational(constantValue : Double) extends ConstantLiveValue(bt.BoxedInexactRational) {
  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    val boxedRationalName = module.nameSource.allocate("schemeInexactRational")

    val boxedRational = bt.BoxedInexactRational.createConstant(
      value=DoubleConstant(constantValue)
    )

    declareBoxedConstant(module)(boxedRationalName, boxedRational)
  }
  
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = {
    case nfi.Float =>
      FloatConstant(constantValue.toFloat)
    case nfi.Double =>
      DoubleConstant(constantValue)
  }
}

object LiveInexactRational {
  def fromConstant(value : Double) : ConstantLiveValue =
    new ConstantLiveInexactRational(value)
} 

