package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveExactInteger(module : IrModuleBuilder)(constantValue : Long) extends ConstantLiveValue(bt.BoxedExactInteger) {
  def genBoxedConstant() : IrConstant = {
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
      FloatConstant(constantValue.toFloat)
    case nfi.Double =>
      DoubleConstant(constantValue.toDouble)
  }
}

private class UnboxedLiveExactInteger(unboxedValue : IrValue, nativeType : nfi.IntType) extends UnboxedLiveValue(bt.BoxedExactInteger, nativeType, unboxedValue) {
  private def genUnboxedInt(state : GenerationState)(targetType : nfi.IntType) : IrValue = {
    val block = state.currentBlock
    val destIrType = IntegerType(targetType.bits)

    if (targetType.bits > nativeType.bits) {
      // Extend ourselves to the dest type's width
      if (nativeType.signed) {
        block.sextTo("sextedInt")(unboxedValue, destIrType)
      }
      else {
        block.zextTo("zextedInt")(unboxedValue, destIrType)
      }
    }
    else if (targetType.bits < nativeType.bits) {
      // Truncate ourselves down
      block.truncTo("truncedint")(unboxedValue, destIrType)
    } else {
      // We're already the right width
      unboxedValue
    }
  }

  override def genUnboxedValue(state : GenerationState)(targetType : nfi.UnboxedType) : Option[IrValue] = targetType match {
    case destNativeType : nfi.IntType =>
      Some(genUnboxedInt(state)(destNativeType))

    case _ => None
  }

  def genBoxedValue(initialState : GenerationState) : (GenerationState, IrValue) = {
    // Cast to a signed int64. This is our preferred integer representation
    val int64Value = genUnboxedInt(initialState)(nfi.Int64)

    // Allocate the cons
    val (state, allocation) = GenConsAllocation(initialState)(1)

    // Initialize it
    val block = state.currentBlock

    val boxedIntCons = allocation.genTypedPointer(state)(0, bt.BoxedExactInteger) 
    bt.BoxedExactInteger.genStoreToValue(block)(int64Value, boxedIntCons)

    (state, boxedIntCons)
  }
}

object LiveExactInteger {
  def fromConstant(module : IrModuleBuilder)(value : Long) : ConstantLiveValue =
    new ConstantLiveExactInteger(module)(value)

  def fromUnboxed(unboxedValue : IrValue, nativeType : nfi.IntType) : LiveValue =
    new UnboxedLiveExactInteger(unboxedValue, nativeType)
  
  def genIntUnboxing(block : IrBlockBuilder)(boxedValue : IrValue, intType : nfi.IntType) : IrValue = {
    val pointerToValue = bt.BoxedExactInteger.genPointerToValue(block)(boxedValue)
    val intValue = block.load("unboxedIntValue")(pointerToValue)

    // Do we need to trunc this value?
    if (intType.bits < 64) {
      block.truncTo("truncedIntValue")(intValue, IntegerType(intType.bits))
    }
    else {
      // No need to truncate
      intValue
    }
  }
  
  def genFpUnboxing(block : IrBlockBuilder)(boxedValue : IrValue, fpType : nfi.FpType) : IrValue = {
    val pointerToValue = bt.BoxedExactInteger.genPointerToValue(block)(boxedValue)
    val intValue = block.load("unboxedIntValue")(pointerToValue)

    // Convert to floating point
    fpType match {
      case nfi.Float =>
        block.sitofp("floatConv")(intValue, FloatType)
      case nfi.Double =>
        block.sitofp("floatConv")(intValue, DoubleType)
    }
  }
} 
