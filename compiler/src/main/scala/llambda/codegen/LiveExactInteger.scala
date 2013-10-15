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
      FloatConstant(constantValue.toFloat)
    case nfi.Double =>
      DoubleConstant(constantValue.toDouble)
  }
}

private class UnboxedLiveExactInteger(unboxedValue : IrValue, nativeType : nfi.IntType) extends UnboxedLiveValue(bt.BoxedExactInteger, nativeType, unboxedValue) {
  def genBoxedValue(state : GenerationState) : IrValue = {
    val block = state.currentBlock

    // Cast to a signed int64. This is our preferred integer representation
    val int64Value = if (nativeType.bits < 64) {
      if (nativeType.signed) {
        block.sextTo("sextedInt64")(unboxedValue, IntegerType(64))
      }
      else {
        block.zextTo("zextedInt64")(unboxedValue, IntegerType(64))
      }
    }
    else {
      unboxedValue
    }

    // Make sure _lliby_box_exact_integer is declared
    val llibyBoxExactIntegerDecl = IrFunctionDecl(
      result=IrFunction.Result(PointerType(bt.BoxedExactInteger.irType)),
      name="_lliby_box_exact_integer",
      arguments=List(IrFunction.Argument(IntegerType(64))),
      attributes=Set(IrFunction.NoUnwind)
    )

    state.module.unlessDeclared(llibyBoxExactIntegerDecl) {
      state.module.declareFunction(llibyBoxExactIntegerDecl)
    }

    block.callDecl(Some("boxedInt"))(llibyBoxExactIntegerDecl, List(int64Value)).get
  }
}

object LiveExactInteger {
  def fromConstant(value : Int) : ConstantLiveValue =
    new ConstantLiveExactInteger(value)

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
