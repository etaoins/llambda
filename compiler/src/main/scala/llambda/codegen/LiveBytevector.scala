package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

private class ConstantLiveBytevector(module : IrModuleBuilder)(constantElements : Seq[Short]) extends ConstantLiveValue(bt.BoxedBytevector) {
  def genBoxedConstant() : IrConstant = {
    val irElements = constantElements.map(IntegerConstant(IntegerType(8), _))
    
    // Make our elements
    val baseName = module.nameSource.allocate("schemeBytevector")

    val elementsName = baseName + ".elements"
    val elementsInitializer = ArrayConstant(IntegerType(8), irElements.toList)
    
    val elementsDef = IrGlobalVariableDef(
      name=elementsName,
      initializer=elementsInitializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(elementsDef)

    val boxedBytevectorName = baseName + ".box"

    val boxedBytevector = bt.BoxedBytevector.createConstant(
      length=IntegerConstant(IntegerType(32), irElements.length),
      data=ElementPointerConstant(IntegerType(8), elementsDef.variable, List(0, 0)))

    declareBoxedConstant(module)(boxedBytevectorName, boxedBytevector)
  }
  
  // There is no unboxed representation of a bytevector currently
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant] = Map.empty
}

object LiveBytevector {
  def fromConstant(module : IrModuleBuilder)(elements : Seq[Short]) : ConstantLiveValue =
    new ConstantLiveBytevector(module)(elements)
} 

