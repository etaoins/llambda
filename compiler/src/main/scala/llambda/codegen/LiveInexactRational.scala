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

private class UnboxedLiveInexactRational(unboxedValue : IrValue, nativeType : nfi.FpType) extends UnboxedLiveValue(bt.BoxedInexactRational, nativeType, unboxedValue) {
  def genBoxedValue(state : GenerationState) : IrValue = {
    val block = state.currentBlock

    // Cast to double. This is our preferred floating point type
    val doubleValue = nativeType match {
      case nfi.Float =>
        block.fpextTo("fpextedDouble")(unboxedValue, DoubleType)
      case nfi.Double =>
        unboxedValue
    }

    // Make sure _lliby_box_inexact_rational is declared
    val llibyBoxInexactRationalDecl = IrFunctionDecl(
      result=IrFunction.Result(PointerType(bt.BoxedInexactRational.irType)),
      name="_lliby_box_inexact_rational",
      arguments=List(IrFunction.Argument(DoubleType)),
      attributes=Set(IrFunction.NoUnwind)
    )

    state.module.unlessDeclared(llibyBoxInexactRationalDecl) {
      state.module.declareFunction(llibyBoxInexactRationalDecl)
    }

    block.callDecl(Some("boxedRational"))(llibyBoxInexactRationalDecl, List(doubleValue)).get
  }
}

object LiveInexactRational {
  def fromConstant(value : Double) : ConstantLiveValue =
    new ConstantLiveInexactRational(value)

  def fromUnboxed(unboxedValue : IrValue, nativeType : nfi.FpType) : LiveValue =
    new UnboxedLiveInexactRational(unboxedValue, nativeType)
} 

