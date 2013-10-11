package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLivePair(constantCar : ConstantLiveValue, constantCdr : ConstantLiveValue) extends ConstantLiveValue(bt.BoxedPair) {
  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    val irCar = constantCar.genCastBoxedConstant(module)(bt.BoxedDatum)
    val irCdr = constantCdr.genCastBoxedConstant(module)(bt.BoxedDatum)
    
    val boxedPairName = module.nameSource.allocate("schemePair")

    val boxedPair = bt.BoxedPair.createConstant(
      car=irCar,
      cdr=irCdr
    )
    
    declareBoxedConstant(module)(boxedPairName, boxedPair)
  }
  
  // This is a structural type. Unboxing it doesn't make sense.
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = Map.empty
}

object LivePair {
  def fromConstant(car : ConstantLiveValue, cdr : ConstantLiveValue) : ConstantLiveValue =
    new ConstantLivePair(car, cdr)
} 

