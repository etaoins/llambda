package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveInexactRational(module : IrModuleBuilder)(constantValue : Double) extends ConstantLiveValue(bt.BoxedInexactRational) {
  def genBoxedConstant() : IrConstant = {
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
  def genBoxedValue(initialState : GenerationState) : (GenerationState, IrValue) = {
    val startBlock = initialState.currentBlock

    // Cast to double. This is our preferred floating point type
    val doubleValue = nativeType match {
      case nfi.Float =>
        startBlock.fpextTo("fpextedDouble")(unboxedValue, DoubleType)
      case nfi.Double =>
        unboxedValue
    }
    
    // Allocate the cons
    val (state, allocation) = GenConsAllocation(initialState)(1)

    // Initialize it
    val exitBlock = state.currentBlock

    val boxedRationalCons = allocation.genTypedPointer(state)(0, bt.BoxedInexactRational) 
    bt.BoxedInexactRational.genStoreToValue(exitBlock)(doubleValue, boxedRationalCons)

    (state, boxedRationalCons)
  }
}

object LiveInexactRational {
  def fromConstant(module : IrModuleBuilder)(value : Double) : ConstantLiveValue =
    new ConstantLiveInexactRational(module)(value)

  def fromUnboxed(unboxedValue : IrValue, nativeType : nfi.FpType) : LiveValue =
    new UnboxedLiveInexactRational(unboxedValue, nativeType)
  
  def genUnboxing(block : IrBlockBuilder)(boxedValue : IrValue, fpType : nfi.FpType) : IrValue = {
    val pointerToValue = bt.BoxedInexactRational.genPointerToValue(block)(boxedValue)
    val fpValue = block.load("unboxedFpValue")(pointerToValue)

    fpType match {
      case nfi.Float =>
        // This needs to be 
        block.fptruncTo("truncedFpValue")(fpValue, FloatType)
      case nfi.Double =>
        fpValue
    }
  }
} 

