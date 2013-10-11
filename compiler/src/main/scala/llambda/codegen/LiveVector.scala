package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveVector(constantElements : Seq[ConstantLiveValue]) extends ConstantLiveValue(bt.BoxedVector) {
  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    val irElements = constantElements.map(_.genCastBoxedConstant(module)(bt.BoxedDatum))
    
    // Make our elements
    val baseName = module.nameSource.allocate("schemeVector")

    val elementsName = baseName + ".elements"
    val elementsInitializer = ArrayConstant(PointerType(bt.BoxedDatum.irType), irElements.toList)
    
    val elementsDef = IrGlobalVariableDef(
      name=elementsName,
      initializer=elementsInitializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(elementsDef)

    val boxedVectorName = baseName + ".box"

    val boxedVector = bt.BoxedVector.createConstant(
      length=IntegerConstant(IntegerType(32), irElements.length),
      elements=ElementPointerConstant(PointerType(bt.BoxedDatum.irType), elementsDef.variable, List(0, 0)))

    declareBoxedConstant(module)(boxedVectorName, boxedVector)
  }
  
  // This is a structural type. Unboxing it doesn't make sense.
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = Map.empty
}

object LiveVector {
  def fromConstant(elements : Seq[ConstantLiveValue]) : ConstantLiveValue =
    new ConstantLiveVector(elements)
} 



